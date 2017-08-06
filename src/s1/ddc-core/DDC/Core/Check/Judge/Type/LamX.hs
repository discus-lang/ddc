{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Judge.Type.LamX
        (checkLamX)
where
import DDC.Core.Check.Judge.Type.Sub
import DDC.Core.Check.Judge.Type.Base
import qualified DDC.Type.Sum   as Sum


-- | Check a lambda abstraction.
checkLamX :: Checker a n

checkLamX !table !ctx mode _demand xx
 = case xx of
        XAbs a m1 x2
          -> checkLam table a ctx m1 x2 mode

        _ -> error $ unlines
                  [ "ddc-core.checkLamX: no match"
                  , show xx]


-- When reconstructing the type of a lambda abstraction,
--  the formal parameter must have a type annotation: eg (λ(v : T) -> x2)
checkLam !table !a !ctx !m1 !x2 !Recon
 = do
        let config      = tableConfig table
        let xx          = XAbs a m1 x2

        -- Check the parameter ------------------
        let t1          = typeOfParam m1

        -- The formal parameter must have a type annotation.
        when (isBot t1)
         $ throw $ ErrorAbsParamUnannotated a (bindOfParam m1)

        -- Determine the kind of the parameter.
        (t1', k1, _)    <- checkTypeM config ctx UniverseSpec t1 Recon
        let m1'         =  replaceTypeOfParam t1' m1

        -- Check the body -----------------------
        -- Reconstruct a type for the body, under the extended environment.
        let (ctx', pos1) = markContext ctx
        let ctx1         = pushType (bindOfParam m1) ctx'

        -- It doesn't matter what we set the demand to at this point
        -- because the 'Recon' mode doesn't use it. We'll just set it
        -- like the other modes to avoid confusion.
        (x2', t2, e2, ctx2)
         <- tableCheckExp table table ctx1 Recon DemandRun x2

        let e2_crush
                = Sum.fromList kEffect
                [ crushEffect (contextEnvT ctx2) (TSum e2)]

        -- The body of the function must produce data.
        (_, k2, _)      <- checkTypeM config ctx2 UniverseSpec t2 Recon
        when (not $ isDataKind k2)
         $ throw $ ErrorMismatch a k2 kData xx

        -- Cut the bound type and elems under it from the context.
        let ctx_cut     = popToPos pos1 ctx2

        -- Build result type --------------------
        -- Build the resulting function type.
        --   The way the effect and closure term is captured depends on
        --   the configuration flags.
        (xAbs', tAbs)
         <- makeFunction config a xx m1' k1 x2' t2 e2_crush

        return  ( xAbs'
                , tAbs
                , Sum.empty kEffect
                , ctx_cut)


-- When synthesizing the type of a lambda abstraction
--   we produce a type (?1 -> ?2) with new unification variables.
checkLam !table !a !ctx !m1 !x2 !(Synth {})
 = do
        ctrace  $ vcat
                [ text "*>  Lam SYNTH"
                , text "    in  bind = " <+> ppr m1
                , empty ]

        let config      = tableConfig table
        let xx          = XAbs a m1 x2

        -- Check the parameter ------------------
        let t1          = typeOfParam m1

        -- If there isn't an existing annotation then make an existential.
        (m1', _t1', k1, ctx1)
         <- if isHoleT config t1
             then do
                -- There is no annotation at all, so make an existential.
                -- Missing anotations are assumed to have kind Data.
                i1      <- newExists kData
                let t1'  = typeOfExists i1
                let m1'  = replaceTypeOfParam t1' m1
                let ctx1 = pushExists i1 ctx
                return (m1', t1', kData, ctx1)

             else do
                -- Check the existing annotation.
                --   This also turns explit holes ? into existentials.
                --   The parameter must have Data or Witness kind,
                --   which is checked by 'makeFunctionType' below.
                (t1', k1, ctx1)
                        <- checkTypeM config ctx UniverseSpec t1 (Synth [])
                let m1' = replaceTypeOfParam t1' m1
                return (m1', t1', k1, ctx1)

        -- Check the body -----------------------
        -- Make an existential for the result type.
        -- The type of a function abstraction has kind Data.
        i2              <- newExists kData
        let t2          = typeOfExists i2

        -- Push the existential for the result,
        -- and parameter type onto the context.
        let (ctx2, pos1) = markContext
                         $ pushExists i2 ctx1
        let ctx3         = pushType   (bindOfParam m1') ctx2

        -- Check the body against the existential for it.
        --   Set the demand to 'Run' to force out any suspensions.
        --   We'll box them up again just underneath the lambda
        --   so that the effects from multiple computations get combined.
        (x2', t2', e2, ctx4)
         <- tableCheckExp table table ctx3 (Check t2) DemandRun x2

        let e2_crush
                = Sum.fromList kEffect
                [ crushEffect (contextEnvT ctx4) (TSum e2)]

        -- Force the kind of the body to be Data.
        --   This constrains the kind of polymorpic variables that are used
        --   as the result of a function, like with (\x. x).
        --   We know \x. can't bind a witness here.
        t2''     <- applyContext ctx4 t2'
        (_, _, ctx5)
         <- checkTypeM config ctx4 UniverseSpec t2'' (Check kData)

        -- Build the result type -------------
        -- If the kind of the parameter is unconstrained then default it
        -- to Data. This handles  "/\f. \(a : f Int#). ()"
        k1'     <- applyContext ctx5 k1
        (k1'', ctx6)
         <- if isTExists k1'
             then do
                ctx6    <- makeEqT config ctx5 k1' kData
                        $  ErrorMismatch a     k1' kData xx

                k1''    <- applyContext ctx6 k1'

                return (k1'', ctx6)

             else do
                return (k1', ctx5)

        -- Cut the bound type and elems under it from the context.
        let ctx_cut     = popToPos pos1 ctx6

        -- Build the resulting function type.
        --  This switches on the kind of the argument, so we need to apply
        --  the context to 'k1' to ensure it has all available information.
        (xAbs', tAbs)
         <- makeFunction
                config a (XAbs a m1' x2)
                m1' k1''
                x2' t2' e2_crush

        ctrace  $ vcat
                [ text "*<  Lam SYNTH"
                , text "    in  bind = " <+> ppr m1
                , text "    out type = " <+> ppr tAbs
                , indent 4 $ ppr ctx
                , indent 4 $ ppr ctx_cut
                , empty ]

        return  ( xAbs'
                , tAbs
                , Sum.empty kEffect
                , ctx_cut)


-- When checking type type of a lambda abstraction against an existing
--   functional type we allow the formal paramter to be missing its
--   type annotation, and in this case we replace it with the expected type.
checkLam !table !a !ctx !m1 !x2 !(Check tExpected)
 | Just (tX1, tX2)      <- takeTFun tExpected
 = do
        ctrace  $ vcat
                [ text "*>  Lam CHECK"
                , text "    in bind =" <+> ppr m1
                , text "    in type =" <+> ppr tExpected
                , empty ]

        let config      = tableConfig table
        let xx          = XAbs a m1 x2

        -- Check the parameter ------------------
        let t1          = typeOfParam m1

        -- If the parameter has no type annotation at all then we can
        --   use the expected type we were passed down from above.
        -- If it does have an annotation, then the annotation also needs
        --   to match the expected type.
        (m1', t1', ctx0)
         <- if isHoleT config t1
             then
                return  (replaceTypeOfParam tX1 m1, tX1, ctx)
             else do
                ctx0    <- makeEqT config ctx t1 tX1
                        $  ErrorMismatch a    t1 tExpected (XAbs a m1 x2)
                return  (m1, t1, ctx0)

        -- Check the body ----------------------
        -- Check the body of the abstraction under the extended environment.
        let (ctx', pos1) = markContext ctx0
        let ctx1         = pushType (bindOfParam m1') ctx'

        -- Check the body against the type we have for it.
        (x2', t2, e2, ctx2)
         <- case takeTSusp tX2 of

             -- If we're implicitly boxing bodies and we're expecting a
             -- suspension, then check the body against the type of the
             -- result of the suspension.
             Just (e2Expected, t2Expected)
              |  configImplicitBox config
              ,  not $ isXCastBox x2
              -> do
                    -- Check the body against the expected result type of the
                    -- suspension.
                    (x2', t2', es2Actual, ctx2)
                      <- tableCheckExp table table ctx1 (Check t2Expected) DemandRun x2

                    let es2Actual_crushed
                          = Sum.fromList kEffect
                          [ crushEffect (contextEnvT ctx2) (TSum es2Actual)]

                    -- The expected effect in the suspension could have been an
                    -- existential, so we need to unify it against the reconstructed
                    -- effect to instantiate it.
                    let e2Actual_crushed = TSum es2Actual_crushed
                    ctx2' <- makeEqT config ctx2 e2Expected e2Actual_crushed
                          $  ErrorMismatch  a    e2Actual_crushed e2Expected x2

                    return (x2', t2', es2Actual_crushed, ctx2')

             _
              -> do
                    (x2', t2', es2Actual, ctx2)
                      <- tableCheckExp table table ctx1 (Check tX2) DemandNone x2

                    let es2Actual_crushed
                          = Sum.fromList kEffect
                          [ crushEffect (contextEnvT ctx2) (TSum es2Actual)]

                    return (x2', t2', es2Actual_crushed, ctx2)

        -- Force the kind of the body to be Data.
        --   This constrains the kind of polymorpic variables that are used
        --   as the result of a function, like with (\x. x).
        --   We know \x. can't bind a witness here.
        t2' <- applyContext ctx2 t2
        (_, _, ctx3)
            <- checkTypeM config ctx2 UniverseSpec t2' (Check kData)

        -- Make the result type -----------------
        -- If the kind of the parameter is unconstrained then default it
        -- to Data. This handles  "/\f. \(a : f Int#). ()"
        (_, k1, _)      <- checkTypeM config ctx3 UniverseSpec t1' (Synth [])
        k1'             <- applyContext ctx3 k1
        (k1'', ctx4)
         <- if isTExists k1'
             then do
                ctx4    <- makeEqT config ctx3 k1' kData
                        $  ErrorMismatch a     k1' kData xx

                k1''    <- applyContext ctx4 k1'

                return (k1'', ctx4)

             else do
                return (k1', ctx3)


        -- Build the resulting function type.
        --  This switches on the kind of the argument, so we need to apply
        --  the context to 'k1' to ensure it has all available information.
        (xAbs', tAbs)
         <- makeFunction
                config a (XAbs a m1' x2)
                m1' k1''
                x2' t2  e2


        -- Ensure that the final type matches the one we expected.
        --   The expected type may have had an existential for the parameter,
        --   which we want to unify with any type annotation that was on
        --   the abstraction.
        --
        --   The `makeFunction` can also insert implicit box casts, so we
        --   need to check that the result of doing this is as expected.
        --
        ctx5    <- makeEqT config ctx4 tAbs tExpected
                $  ErrorMismatch  a    tAbs tExpected xx

        tAbs'   <- applyContext ctx4 tAbs

        -- Cut the bound type and elems under it from the context.
        let ctx_cut     = popToPos pos1 ctx5

        ctrace  $ vcat
                [ text "*<  Lam CHECK"
                , text "    in  type: " <> ppr tExpected
                , text "    out type: " <> ppr tAbs'
                , indent 4 $ ppr ctx
                , indent 4 $ ppr ctx_cut
                , empty ]

        return  ( xAbs'
                , tAbs'
                , Sum.empty kEffect
                , ctx_cut)


-- The expected type is not a functional type, yet we have a lambda
-- abstraction. Fall through to the subsumtion checker which will
-- throw the error message.
checkLam !table !a !ctx !m1 !x2 !(Check tExpected)
 = do
        ctrace  $ vcat
                [ text "*>  Lam CHECK (not function)"
                , text "    m1        = " <> ppr m1
                , text "    x2        = " <> (indent 4 $ ppr x2)
                , text "    tExpected = " <> ppr tExpected
                , empty ]

        checkSub table a ctx DemandNone (XAbs a m1 x2) tExpected


-------------------------------------------------------------------------------
-- | Construct a function type with the given effect and closure.
--
--   Whether this is a witness or data abstraction depends on the kind
--   of the parameter type.
--
--   For data abstractions, the way the effect and closure is handled
--   is set by the Config, which depends on the specific language fragment
--   that we're checking.
--
makeFunction
        :: (Show n, Ord n)
        => Config n             -- ^ Type checker config.
        -> a                    -- ^ Annotation for error messages.
        -> Exp  a n             -- ^ Expression for error messages.
        -> Param n              -- ^ Parameter of the function.
        -> Kind n               -- ^ Kind of the parameter.
        -> Exp  (AnTEC a n) n   -- ^ Body of the function.
        -> Type n               -- ^ Result type of the function.
        -> TypeSum n            -- ^ Sum of effects of the body expression.
        -> CheckM a n (Exp (AnTEC a n) n, Type n)

makeFunction config a xx mParam kParam xBody tBody eBody
 | isTExists kParam
 = throw $ ErrorAbsBindBadKind a xx (typeOfParam mParam) kParam

 | not (kParam == kData) && not (kParam == kWitness)
 = throw $ ErrorAbsBindBadKind a xx (typeOfParam mParam) kParam

 | otherwise
 = do
        -- Get the universe the parameter value belongs to.
        let Just uniParam
                = universeFromType2 kParam

        -- The effects due to evaluating the body that are
        -- captured by this abstraction.
        let eCaptured
                -- If we're not tracking effect information then just drop it
                -- on the floor.
                | not  $ configTrackedEffects config    = tBot kEffect
                | otherwise                             = TSum eBody

        -- Data abstraction where the function constructor for the language
        -- fragment does not suport latent effects or closures.
        if (    kParam == kData
                && eCaptured == tBot kEffect)
         then let tAbs  = makeTFunParams [mParam] tBody
                  aAbs  = AnTEC tAbs (tBot kEffect) (tBot kClosure) a
              in  return ( XAbs aAbs mParam xBody
                         , tAbs)

        -- Witness abstractions must always be pure,
        --  but closures are passed through.
        else if (  kParam == kWitness
                && eCaptured == tBot kEffect)
         then let tAbs  = tImpl (typeOfParam mParam) tBody
                  aAbs  = AnTEC tAbs (tBot kEffect) (tBot kClosure) a
              in  return ( XAbs aAbs mParam xBody
                         , tAbs)

        -- Handle ImplicitBoxBodies
        --   Evaluating the given body causes an effect, but the body of an
        --   abstraction must be pure. Automatically box up the body to build
        --   a suspension that we can abstract over. We justify the fact that
        --   inserting this cast is valid because if we didn't the program
        --   would be ill-typed, as the next case it to throw an error.
        else if (   configImplicitBox config
                && (eCaptured /= tBot kEffect))
         then
              case takeTSusp tBody of

                -- The body itself does not produce another suspension,
                -- so we can just box it up.
                Nothing
                 -> let tBodySusp = tSusp eCaptured tBody
                        aBox      = AnTEC tBodySusp (tBot kEffect) (tBot kClosure) a

                        tAbs      = makeTFunParams [mParam] tBodySusp
                        aAbs      = AnTEC tAbs      (tBot kEffect) (tBot kClosure) a

                    in  return  ( XAbs aAbs mParam (XCast aBox CastBox xBody)
                                , tAbs)

                -- The body itself produces another suspension.
                -- Instead boxing this to form a result of type:
                --    S eCaptured (S eResult tResult)
                --
                -- we instead run the inner suspension and re-box it,
                -- so that we have a single suspension that includes both effects:
                --    S (eCaptured + eResult) tResult
                --
                Just (eSusp, tResult)
                 -> let aRun      = AnTEC tResult eSusp (tBot kClosure) a

                        eTotal    = tSum kEffect [eSusp, eCaptured]
                        tBodySusp = tSusp eTotal tResult
                        aBox      = AnTEC tBodySusp (tBot kEffect) (tBot kClosure) a

                        tAbs      = makeTFunParams [mParam] tBodySusp
                        aAbs      = AnTEC tAbs      (tBot kEffect) (tBot kClosure) a

                    in  return  ( XAbs aAbs mParam
                                        $ XCast aBox CastBox
                                        $ XCast aRun CastRun
                                        $ xBody
                                , tAbs)

        -- We don't have a way of forming a function with an impure effect.
        else if (eCaptured /= tBot kEffect)
         then   throw $ ErrorAbsNotPure  a xx uniParam eCaptured

        -- One of the above error reporting cases should have fired already.
        else    error $ "ddc-core.makeFunctionType: is broken."


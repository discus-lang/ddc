
module DDC.Core.Check.Judge.Type.Abs
        (checkAbs)
where
import DDC.Core.Check.Judge.Type.Sub
import DDC.Core.Check.Judge.Type.Base
import qualified DDC.Type.Sum   as Sum
import qualified Data.Set       as Set


-- Dispatch -------------------------------------------------------------------
checkAbs :: Checker a n
checkAbs !table !ctx (XLAM a b1 x2) mode
 = checkAbsLAM table ctx a b1 x2 mode
         
checkAbs !table !ctx (XLam a b1 x2) mode
 = checkAbsLam table a ctx b1 x2 mode

checkAbs _ _ _ _
        = error "ddc-core.checkAbs: no match."


-- AbsLAM ---------------------------------------------------------------------
checkAbsLAM !table !ctx0 a b1 x2 Recon
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table
        let xx          = XLAM a b1 x2

        -- Check the parameter ------------------
        -- The parameter cannot shadow others.
        when (memberKindBind b1 ctx0)
         $ throw $ ErrorLamShadow a xx b1

        -- The parameter must have an explict kind annotation.
        let kA  = typeOfBind b1
        when (isBot kA)
         $ throw $ ErrorLAMParamUnannotated a xx

        (kA', _sA, ctxA)
         <- checkTypeM config kenv ctx0 UniverseKind kA Recon

        -- TODO: check kA has sort Comp or Prop
        let b1'         = replaceTypeOfBind kA' b1

        -- Push the type parameter onto the context.
        let (ctx2, pos1) = markContext ctxA
        let ctx3         = pushKind b1' RoleAbstract ctx2
        let ctx4         = liftTypes 1  ctx3

        
        -- Check the body -----------------------
        (x2', t2, e2, c2, ctx5)
         <- tableCheckExp table table ctx4 x2 Recon
        
        -- Reconstruct the kind of the body.
        (t2', k2, ctx6) 
         <- checkTypeM config kenv ctx5 UniverseSpec t2 Recon
        
        -- The type of the body must have data kind.
        when (not $ isDataKind k2)
         $ throw $ ErrorLamBodyNotData a xx b1 t2' k2

        -- The body of a spec abstraction must be pure.
        when (e2 /= Sum.empty kEffect)
         $ throw $ ErrorLamNotPure a xx UniverseSpec (TSum e2)

        -- Mask closure terms due to locally bound region vars.
        let c2_cut      = Set.fromList
                        $ mapMaybe (cutTaggedClosureT b1)
                        $ Set.toList c2

        -- Cut the bound kind and elems under it from the context.
        let ctx_cut     = lowerTypes 1
                        $ popToPos pos1 ctx6
                                   
        -- Build the result type.
        let tResult     = TForall b1' t2'

        ctrace  $ vcat
                [ text "* LAM Recon"
                , indent 2 $ ppr (XLAM a b1' x2)
                , text "  OUT: " <> ppr tResult
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx_cut 
                , empty ]

        returnX a
                (\z -> XLAM z b1' x2')
                tResult
                (Sum.empty kEffect)
                c2_cut
                ctx_cut


checkAbsLAM !table !ctx0 a b1 x2 Synth
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table
        let xx          = XLAM a b1 x2

        -- Check the parameter ------------------
        -- The parameter cannot shadow others.
        when (memberKindBind b1 ctx0)
         $ throw $ ErrorLamShadow a xx b1

        -- If the annotation is missing then make a new existential for it.
        let kA  = typeOfBind b1
        (kA', _sA, ctxA)
         <- if isBot kA 
             then do   
                iA       <- newExists sComp
                let kA'  = typeOfExists iA
                let ctxA = pushExists   iA ctx0
                return (kA', sComp, ctxA)

             else
                checkTypeM config kenv ctx0 UniverseKind kA Synth

        -- TODO: check kA has sort Comp or Prop
        let b1'         = replaceTypeOfBind kA' b1

        -- Push the type parameter onto the context.
        let (ctx2, pos1) = markContext ctxA
        let ctx3         = pushKind b1' RoleAbstract ctx2
        let ctx4         = liftTypes 1  ctx3

        -- Check the body -----------------------
        (x2', t2, e2, c2, ctx5)
         <- tableCheckExp table table ctx4 x2 Synth
        
        -- Force the kind of the body to be data.
        (_, _, ctx6) 
         <- checkTypeM config kenv ctx5 UniverseSpec 
                (applyContext ctx5 t2) (Check kData)
        
        -- The body of a spec abstraction must be pure.
        when (e2 /= Sum.empty kEffect)
         $ throw $ ErrorLamNotPure a xx UniverseSpec (TSum e2)

        -- Mask closure terms due to locally bound region vars.
        let c2_cut      = Set.fromList
                        $ mapMaybe (cutTaggedClosureT b1)
                        $ Set.toList c2

        -- Cut the bound kind and elems under it from the context.
        let ctx_cut     = lowerTypes 1
                        $ popToPos pos1 ctx6
        
        -- Build the result type.
        let tResult     = TForall b1' t2

        ctrace  $ vcat
                [ text "* LAM Synth"
                , indent 2 $ ppr (XLAM a b1' x2)
                , text "  OUT: " <> ppr tResult
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx_cut 
                , empty ]

        returnX a
                (\z -> XLAM z b1' x2')
                tResult
                (Sum.empty kEffect)
                c2_cut
                ctx_cut


checkAbsLAM !table !ctx0 a b1 x2 (Check (TForall b tBody))
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table
        let xx          = XLAM a b1 x2

        -- Check the parameter ------------------
        -- If the bound variable is named then it cannot shadow
        -- shadow others in the environment.
        when (memberKindBind b1 ctx0)
         $ throw $ ErrorLamShadow a xx b1

        -- If we have an expected kind for the parameter then it needs
        -- to be the same as the one we already have.
        let kA  = typeOfBind b1
        when (  (not $ isBot kA)
             && (not $ equivT kA (typeOfBind  b)))
         $ throw $ ErrorLAMParamUnexpected a xx b1 kA

        -- If both the kind annotation is missing and there is no
        -- expected kind then we need to make an existential for it.
        (kA', _sA, ctxA)
         <- if (isBot kA && isBot (typeOfBind b)) 
             then do
                iA       <- newExists sComp
                let kA'  = typeOfExists iA
                let ctxA = pushExists   iA ctx0
                return (kA', sComp, ctxA)

             else if isBot (typeOfBind b) 
              then do
                checkTypeM config kenv ctx0 UniverseKind kA Synth

              else do
                checkTypeM config kenv ctx0 UniverseKind kA Synth 
                        -- TODO: check against sort of b

        -- TODO: check kA has sort Comp or Prop
        let b1' = replaceTypeOfBind kA' b1

        -- Push the type parameter onto the context.
        let (ctx2, pos1) = markContext ctxA
        let ctx3         = pushKind b1' RoleAbstract ctx2
        let ctx4         = liftTypes 1  ctx3

        -- Check the body -----------------------
        tBody_skol
         <- case takeSubstBoundOfBind b1 of
                Nothing -> return tBody
                Just u1 -> return $ substituteT b (TVar u1) tBody

        (x2', t2, e2, c2, ctx5)
         <- tableCheckExp table table ctx4 x2 (Check tBody_skol)
        
         -- The body of a spec abstraction must have data kind.
        (t2', _k2, ctx6)
         <- checkTypeM config kenv ctx5 UniverseSpec t2 (Check kData)

        -- The body of a spec abstraction must be pure.
        when (e2 /= Sum.empty kEffect)
         $ throw $ ErrorLamNotPure a xx UniverseSpec (TSum e2)

        -- Mask closure terms due to locally bound region vars.
        let c2_cut      = Set.fromList
                        $ mapMaybe (cutTaggedClosureT b1)
                        $ Set.toList c2

        -- Apply context to synthesised type.
        -- We're about to pop the context back to how it was before the 
        -- type lambda, and will information gained from synthing the body.
        let t2_sub      = applyContext ctx6 t2'

        -- Cut the bound kind and elems under it from the context.
        let ctx_cut     = lowerTypes 1
                        $ popToPos pos1 ctx6
        
        -- Build the result type.
        let tResult     = TForall b1' t2_sub

        ctrace  $ vcat
                [ text "* LAM"
                , indent 2 $ ppr (XLAM a b1' x2)
                , text "  OUT: " <> ppr tResult
                , indent 2 $ ppr ctx0
                , indent 2 $ ppr ctx_cut 
                , empty ]

        returnX a
                (\z -> XLAM z b1' x2')
                tResult
                (Sum.empty kEffect)
                c2_cut
                ctx_cut

checkAbsLAM table ctx0 a b1 x2 (Check tExpected)
 = checkSub table a ctx0 (XLAM a b1 x2) tExpected

        
-- AbsLam -----------------------------------------------------------------
-- When reconstructing the type of a lambda abstraction,
--  the formal parameter must have a type annotation: eg (\v : T. x2)
checkAbsLam !table !a !ctx !b1 !x2 !Recon
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table
        let xx          = XLam a b1 x2

        -- Check the parameter ------------------
        let t1          = typeOfBind b1
        
        -- The formal parameter must have a type annotation.
        when (isBot t1)
         $ throw $ ErrorLamParamTypeMissing a xx b1

        -- Determine the kind of the parameter.
        (t1', k1, _)    <- checkTypeM config kenv ctx UniverseSpec t1 Recon
        let b1'         = replaceTypeOfBind t1' b1

        -- Check the body -----------------------
        -- Reconstruct a type for the body, under the extended environment.
        let (ctx', pos1) = markContext ctx
        let ctx1         = pushType b1 ctx'

        (x2', t2, e2, c2, ctx2)
         <- tableCheckExp table table ctx1 x2 Recon

        -- The body of the function must produce data.
        (_, k2, _)      <- checkTypeM config kenv ctx2 UniverseSpec t2 Recon
        when (not $ isDataKind k2)
         $ throw $ ErrorLamBodyNotData a xx b1 t2 k2 

        -- Cut closure terms due to locally bound value vars.
        -- This also lowers deBruijn indices in un-cut closure terms.
        let c2_cut      = Set.fromList
                        $ mapMaybe (cutTaggedClosureX b1)
                        $ Set.toList c2

        -- Cut the bound type and elems under it from the context.
        let ctx_cut     = popToPos pos1 ctx2
        

        -- Build result type --------------------
        -- Build the resulting function type.
        --   The way the effect and closure term is captured depends on
        --   the configuration flags.
        (tResult, cResult)
         <- makeFunctionType config a xx t1 k1 t2 e2 c2_cut
        
        returnX a
                (\z -> XLam z b1' x2')
                tResult 
                (Sum.empty kEffect)
                cResult
                ctx_cut


-- When synthesizing the type of a lambda abstraction
--   we produce a type (?1 -> ?2) with new unification variables.
checkAbsLam !table !a !ctx !b1 !x2 !Synth
 = do   let config      = tableConfig table     
        let kenv        = tableKindEnv table

        -- Check the parameter ------------------
        let t1          = typeOfBind b1

        -- If there isn't an existing annotation then make an existential.
        (b1', t1', ctx1)
         <- if isBot t1
             then do 
                -- There is no annotation at all, 
                --   so make an existential.
                -- Missing anotations are assumed to have kind Data.
                i1      <- newExists kData
                let t1'  = typeOfExists i1
                let b1'  = replaceTypeOfBind t1' b1
                let ctx1 = pushExists i1 ctx
                return (b1', t1', ctx1)
                     
             else do
                -- Check the existing annotation.
                --   This also turns explit holes ? into existentials.
                (t1', _, ctx1)
                        <- checkTypeM config kenv ctx UniverseSpec t1 Synth
                let b1' = replaceTypeOfBind t1' b1
                return (b1', t1', ctx1)


        -- Check the body -----------------------        
        -- Make an existential for the result type.
        i2              <- newExists kData
        let t2          = typeOfExists i2
        
        -- Push the existential for the result, 
        -- and parameter type onto the context.
        let (ctx2, pos1) = markContext 
                         $ pushExists i2 ctx1
        let ctx3         = pushType   b1' ctx2


        -- Check the body against the existential for it.
        (x2', t2', e2, c2, ctx4)
         <- tableCheckExp table table ctx3 x2 (Check t2)

        -- Force the kind of the body to be Data.
        (_, _, ctx5)    <- checkTypeM config kenv ctx4 UniverseSpec 
                                (applyContext ctx4 t2')
                                (Check kData)

        -- Cut the bound type and elems under it from the context.
        let ctx_cut     = popToPos pos1 ctx5

        
        -- Build the result type -------------
        -- Determine the kind of the parameter.
        (_, k1, _)      <- checkTypeM config kenv ctx5 UniverseSpec t1' Recon
        
        -- Cut closure terms due to locally bound value vars.
        -- This also lowers deBruijn indices in un-cut closure terms.
        let c2_cut      = Set.fromList
                        $ mapMaybe (cutTaggedClosureX b1')
                        $ Set.toList c2

        -- Build the resulting function type.
        (tResult, cResult)
         <- makeFunctionType config a (XLam a b1' x2) 
                t1' (applyContext ctx5 k1) 
                t2' e2 c2_cut

                        
        ctrace  $ vcat
                [ text "* Lam Synth"
                , indent 2 $ ppr (XLam a b1' x2)
                , text "  OUT: " <> ppr tResult
                , indent 2 $ ppr ctx
                , indent 2 $ ppr ctx_cut 
                , empty ]

        returnX a
                (\z -> XLam z b1' x2')
                tResult
                (Sum.empty kEffect)
                cResult
                ctx_cut


-- When checking type type of a lambda abstraction against an existing
--   functional type we allow the formal paramter to be missing its
--   type annotation, and in this case we replace it with the expected type.
checkAbsLam !table !a !ctx !b1 !x2 !(Check tXX)
 | Just (tX1, tX2)      <- takeTFun tXX
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table
        
        -- Check the parameter ------------------
        let t1          = typeOfBind b1

        -- If the parameter has no type annotation then we can use the
        --   expected type we were passed down from above.
        -- If it does have an annotation, then it needs to match the
        --   expected type.
        (b1', t1', ctx0) 
         <- if isBot t1             
             then 
                return  (replaceTypeOfBind tX1 b1, tX1, ctx)
             else do
                ctx0    <- makeEq a (ErrorMismatch a tX1 t1 (XLam a b1 x2))
                                ctx t1 tX1 
                return  (b1, t1, ctx0)
                        
        -- Check the body ----------------------
        -- Check the body of the abstraction under the extended environment.
        let (ctx', pos1) = markContext ctx0
        let ctx1         = pushType b1' ctx'

        (x2', t2, e2, c2, ctx2)
         <- tableCheckExp table table ctx1 x2 (Check tX2)

        -- Force the kind of the body to be Data.
        (_, _, ctx3)    <- checkTypeM config kenv ctx2 UniverseSpec 
                                (applyContext ctx2 t2) 
                                (Check kData)

        -- Cut the bound type and elems under it from the context.
        let ctx_cut     = popToPos pos1 ctx3
        

        -- Make the result type -----------------
        -- Determine the kind of the parameter.
        (_, k1, _)      <- checkTypeM config kenv ctx3 UniverseSpec t1' Recon

        -- Cut closure terms due to locally bound value vars.
        -- This also lowers deBruijn indices in un-cut closure terms.
        let c2_cut      = Set.fromList
                        $ mapMaybe (cutTaggedClosureX b1)
                        $ Set.toList c2

        -- Build the resulting function type.
        (tResult, cResult)
         <- makeFunctionType config a (XLam a b1' x2) 
                t1' (applyContext ctx3 k1) 
                t2 e2 c2_cut

       
        ctrace  $ vcat 
                [ text "* Lam Check"
                , indent 2 $ ppr (XLam a b1' x2)
                , text "  IN:  " <> ppr tXX
                , text "  OUT: " <> ppr tResult
                , indent 2 $ ppr ctx
                , indent 2 $ ppr ctx_cut
                , empty ]

        returnX a
                (\z -> XLam z b1' x2')
                tResult
                (Sum.empty kEffect)
                cResult
                ctx_cut

checkAbsLam !table !a !ctx !b1 !x2 !(Check tExpected)
 = checkSub table a ctx (XLam a b1 x2) tExpected


-------------------------------------------------------------------------------
-- | Construct a function-like type with the given effect and closure.
--
--   Whether this is a witness or data abstraction depends on the kind
--   of the parameter type.
--
--   For data abstractions, the way the effect and closure is handled
--   is set by the Config, which depends on the specific language fragment
--   that we're checking.
--
makeFunctionType 
        :: (Show n, Ord n)
        => Config n              -- ^ Type checker config.
        -> a                     -- ^ Annotation for error messages.
        -> Exp a n               -- ^ Expression for error messages.
        -> Type n                -- ^ Parameter type of the function.
        -> Kind n                -- ^ Kind of the parameter.
        -> Type n                -- ^ Result type of the function.
        -> TypeSum n             -- ^ Effect sum.
        -> Set (TaggedClosure n) -- ^ Closure terms.
        -> CheckM a n (Type n, Set (TaggedClosure n))

makeFunctionType config a xx t1 k1 t2 e2 c2
 | isTExists k1
 = throw $ ErrorLamBindBadKind a xx t1 k1

 | not (k1 == kData) && not (k1 == kWitness)
 = throw $ ErrorLamBindBadKind a xx t1 k1

 | otherwise
 = do   
        -- Get the universe the parameter value belongs to.
        let Just uniParam    = universeFromType2 k1

        -- Trim the closure before we annotate the returned function
        -- type with it. This should always succeed because trimClosure
        -- only returns Nothing if the closure is miskinded, and we've
        -- already already checked that.
        let c2_captured
                -- If we're not tracking closure information then just drop it 
                -- on the floor.
                | not  $ configTrackedClosures config   = tBot kClosure
                | otherwise
                = let Just c = trimClosure $ closureOfTaggedSet c2 in c

        let e2_captured
                -- If we're not tracking effect information then just drop it 
                -- on the floor.
                | not  $ configTrackedEffects config    = tBot kEffect
                | otherwise                             = TSum e2


        -- Data abstraction where the function type for the language fragment
        -- supports latent effects and closures.
        if      (  k1 == kData
                && configFunctionalEffects  config
                && configFunctionalClosures config)
         then   return  ( tFunEC t1 e2_captured c2_captured t2
                        , c2)

        -- Data abstraction where the function constructor for the language
        -- fragment does not suport latent effects or closures.
        else if (  k1 == kData
                && e2_captured == tBot kEffect
                && c2_captured == tBot kClosure)
         then   return  ( tFun t1 t2
                        , Set.empty)

        -- Witness abstractions must always be pure,
        --  but closures are passed through.
        else if (  k1 == kWitness
                && e2_captured == tBot kEffect)
         then   return  ( tImpl t1 t2
                        , c2 )

        -- We don't have a way of forming a function with an impure effect.
        else if (e2_captured /= tBot kEffect)
         then   throw $ ErrorLamNotPure  a xx uniParam e2_captured

        -- We don't have a way of forming a function with an non-empty closure.
        else if (c2_captured /= tBot kClosure)
         then   throw $ ErrorLamNotEmpty a xx uniParam c2_captured

        -- One of the above error reporting cases should have fired already.
        else    error $ "ddc-core.makeFunctionType: is broken."



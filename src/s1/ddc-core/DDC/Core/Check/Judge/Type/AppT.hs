{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Judge.Type.AppT
        (checkAppT)
where
import DDC.Core.Check.Judge.Type.Sub
import DDC.Core.Check.Judge.Type.Base


-- | Check a spec application.
checkAppT :: Checker a n

checkAppT !table !ctx0 Recon demand
        xx@(XApp aApp xFn (RType tArg))
 = do
        let config      = tableConfig table

        -- Check the functional expression.
        (xFn', tFn, effsFn, ctx1)
         <- tableCheckExp table table ctx0 Recon demand xFn

        -- Check the argument.
        (tArg', kArg, ctx2)
         <- checkTypeM    config ctx1 UniverseSpec tArg Recon

        -- Determine the type of the result.
        --  The function must have a quantified type, which we then instantiate
        --  with the type argument.
        tResult
         <- case tFn of
                TForall b11 t12
                 | typeOfBind b11 == kArg
                 -> return $ substituteT b11 tArg' t12

                 | otherwise
                 ->  throw $ ErrorMismatch aApp (typeOfBind b11) tArg' xx

                _ -> throw $ ErrorAppNotFun   aApp xx tFn

        -- We don't need to substitute into the effect of x1 (effs1)
        -- because the body of a type abstraction is required to be pure.

        -- We don't need to substitute into the closure either, because
        -- the bound type variable is not visible outside the abstraction.
        -- thus we can't be sharing objects that have it in its type.

        -- Build an annotated version of the type application.
        let aApp' = AnTEC tResult (TSum effsFn)  (tBot kClosure) aApp
        let xx'   = XApp aApp' xFn' (RType tArg')

        ctrace  $ vcat
                [ text "* APP Recon"
                , text "      xx : " <+> ppr xx
                , text "      xx': " <+> ppr xx'
                , text "      tFn: " <+> ppr tFn
                , text "     tArg: " <+> ppr tArg
                , text "  tResult: " <+> ppr tResult
                , indent 2 $ ppr ctx2
                , empty ]

        returnX aApp
                (\z -> XApp z xFn' (RType tArg'))
                tResult effsFn ctx2

checkAppT !table !ctx0 (Synth {}) demand
        xx@(XApp aApp xFn (RType tArg))
 = do
        -- Check the functional expression.
        (xFn', tFn, effsFn, ctx1)
         <- tableCheckExp table table ctx0 (Synth []) demand xFn

        -- Apply the type argument to the type of the function.
        tFn' <- applyContext ctx1 tFn
        (tResult, tArg', _kArg, ctx2)
             <- synthAppArgT table aApp xx ctx1 tFn' tArg

        -- Build an annotated version of the type application.
        let aApp' = AnTEC tResult (TSum effsFn)  (tBot kClosure) aApp
        let xx'   = XApp aApp' xFn' (RType tArg')

        ctrace  $ vcat
                [ text "* APP Synth"
                , text "      xx : " <+> ppr xx
                , text "      xx': " <+> ppr xx'
                , text "      tFn: " <+> ppr tFn
                , text "     tArg: " <+> ppr tArg
                , text "  tResult: " <+> ppr tResult
                , indent 2 $ ppr ctx2
                , empty ]

        returnX aApp
                (\z -> XApp z xFn' (RType tArg'))
                tResult effsFn ctx2


checkAppT !table !ctx0 (Check tExpected) demand
        xx@(XApp aApp _ (RType _))
 =      checkSub table aApp ctx0 demand xx tExpected

checkAppT _ _ _ _ _
 = error "ddc-core.checkAppT: no match"


-------------------------------------------------------------------------------
-- | Synthesise the type of a polymorphic function applied to its type argument.
synthAppArgT
        :: (Show n, Ord n, Pretty n)
        => Table a n
        -> a                    -- Annot for error messages.
        -> Exp a n              -- Expression for error messages.
        -> Context n            -- Current context.
        -> Type n               -- Type of functional expression.
        -> Type n               -- Type argument.
        -> CheckM a n
                ( Type n        -- Type of result
                , Type n        -- Checked type argument.
                , Kind n        -- Kind of type argument.
                , Context n)    -- Result context

synthAppArgT table a xx ctx0 tFn tArg

 -- Rule (AppT Synth exists)
 --  Functional type is an existential.
 --
 --  Although we know the functional part should have a quantified type,
 --  we can't infer a type for the result because we would need to represent
 --  a delayed substitution of a type into an existential. The rule would be
 --  as follows:
 --
 --    Env0[?2, ?1, ?0 = [a : ?1]. ?2] |- t2 <= ?1 -| Env1
 --   -----------------------------------------------------
 --      Env0[?0] |- ?0 * t2 => ?2 [t2/a] -| Env1
 --
 --  .. but we can't represent the (?2 [t2/a]) part. This is an inherent
 --  limitation of our type inference algorithm.
 --
 | Just _               <- takeExists tFn
 = do   throw $ ErrorAppCannotInferPolymorphic a xx


 -- Rule (AppT Synth Forall)
 --  The function already has a quantified type, so we can instantiate it
 --  with the supplied type argument.
 | TForall b11 t12      <- tFn
 = do   let config      = tableConfig table

        -- The kind of the argument must match the annotation on the quantifier.
        (tArg', kArg, ctx1)
         <- checkTypeM config ctx0 UniverseSpec tArg
                (Check (typeOfBind b11))

        -- Instantiate the type of the function with the type argument.
        let tResult = substituteT b11 tArg' t12

        return (tResult, tArg', kArg, ctx1)


 | otherwise
 = throw $ ErrorAppNotFun a xx tFn


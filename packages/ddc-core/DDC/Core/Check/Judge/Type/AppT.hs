
module DDC.Core.Check.Judge.Type.AppT
        (checkAppT)
where
import DDC.Core.Check.Judge.Type.Sub
import DDC.Core.Check.Judge.Type.Base
import qualified Data.Set       as Set


-- | Check a spec expression application.
--
--   Note: We don't need to substitute into the effect of x1 (effs1)
--         because the body of a type abstraction is required to be pure.
-- 
--         We don't need to substitute into the closure either, because
--         the bound type variable is not visible outside the abstraction.
--         thus we can't be sharing objects that have it in its type.
--
checkAppT :: Checker a n

checkAppT !table !ctx0 xx@(XApp aApp xFn (XType aArg tArg)) Recon
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table

        -- Check the functional expression.
        (xFn', tFn, effsFn, closFn, ctx1) 
         <- tableCheckExp table table ctx0 xFn Recon

        -- Check the argument.
        (tArg', kArg, ctx2)
         <- checkTypeM config kenv ctx1 UniverseSpec tArg Recon
                        
        -- Take any Use annots from a region arg.
        --  This always matches because we just checked tArg.
        let Just t2_clo = taggedClosureOfTyArg kenv ctx2 tArg'

        -- Determine the type of the result.
        --  The function must have a quantified type, which we then instantiate
        --  with the type argument.
        tResult
         <- case tFn of
                TForall b11 t12
                 | typeOfBind b11 == kArg
                 -> return $ substituteT b11 tArg' t12

                 | otherwise
                 ->  throw $ ErrorAppMismatch aApp xx (typeOfBind b11) tArg'

                _ -> throw $ ErrorAppNotFun   aApp xx tFn

        -- Build an annotated version of the type application.
        let aApp'  = AnTEC tResult (TSum effsFn)  (closureOfTaggedSet closFn) aApp  
        let aArg'  = AnTEC kArg    (tBot kEffect) (tBot kClosure) aArg
        let xx'    = XApp aApp' xFn' (XType aArg' tArg')

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
                (\z -> XApp z xFn' (XType aArg' tArg'))
                tResult effsFn (closFn `Set.union` t2_clo)
                ctx2

checkAppT !table !ctx0 xx@(XApp aApp xFn (XType aArg tArg)) Synth
 = do   let kenv        = tableKindEnv table

        -- Check the functional expression.
        (xFn', tFn, effsFn, closFn, ctx1) 
         <- tableCheckExp table table ctx0 xFn Synth

        -- Apply the type argument to the type of the function.
        (tResult, tArg', kArg, ctx2)
         <- synthAppArgT table aApp xx ctx1 
                (applyContext ctx1 tFn) tArg
        
        -- Take any Use annots from a region arg.
        --  This always matches because we just checked tArg.
        let Just t2_clo = taggedClosureOfTyArg kenv ctx2 tArg'

        -- Build an annotated version of the type application.
        let aApp'  = AnTEC tResult (TSum effsFn)  (closureOfTaggedSet closFn) aApp  
        let aArg'  = AnTEC kArg    (tBot kEffect) (tBot kClosure) aArg
        let xx'    = XApp aApp' xFn' (XType aArg' tArg')

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
                (\z -> XApp z xFn' (XType aArg' tArg'))
                tResult effsFn (closFn `Set.union` t2_clo)
                ctx2


checkAppT !table !ctx0 xx@(XApp aApp _ (XType _ _)) (Check tExpected)
 =      checkSub table aApp ctx0 xx tExpected

checkAppT _ _ _ _
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

 -- Rule (AppT Synth Forall)
 --  The function already has a quantified type, so we can instantiate it 
 --  with the supplied type argument.
 | TForall b11 t12      <- tFn
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table

        -- The kind of the argument must match the annotation on the quantifier.
        (tArg', kArg, ctx1)
         <- checkTypeM config kenv ctx0 UniverseSpec tArg 
                (Check (typeOfBind b11))

        -- Instantiate the type of the function with the type argument.
        let tResult = substituteT b11 tArg' t12

        return (tResult, tArg', kArg, ctx1)

 | otherwise
 = throw $ ErrorAppNotFun a xx tFn



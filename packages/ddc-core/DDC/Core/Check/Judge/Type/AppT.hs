
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
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table

        -- Check the functional expression.
        (xFn', tFn, effsFn, closFn, ctx1) 
         <- tableCheckExp table table ctx0 xFn Synth

        -- Check the type argument.
        (tArg', kArg, ctx2)
         <- case tArg of
                -- TODO: we shouldn't need to do this hole check here.
                -- Make the other judgments, Eq, Sub etc convert holes to existentials.
                -- Those judgments are only used in bidir mode, so always ok to 
                -- introduce new ones.
                TVar (UPrim n kArg)
                 |  Just isHole <- configNameIsHole config 
                 ,  isHole n
                 -> do  i2        <- newExists kArg
                        let tArg' = typeOfExists i2
                        let ctx2  = pushExists i2 ctx1
                        return  (tArg', kArg, ctx2)

                _ -> checkTypeM config kenv ctx1 UniverseSpec tArg Synth
        
        -- Take any Use annots from a region arg.
        --  This always matches because we just checked tArg.
        let Just t2_clo = taggedClosureOfTyArg kenv ctx2 tArg'

        --  The function must have a quantified type, which we then instantiate
        --  with the type argument.
        (tResult, ctx3)    
         <- case tFn of
                TForall b11 t12
                 -> do  -- The kind of the argument must match the annotation on the quantifier.
                        ctx3    <- makeEq config aApp 
                                        (ErrorAppMismatch aApp xx (typeOfBind b11) tArg')
                                        ctx2 (typeOfBind b11) kArg

                        let tResult = substituteT b11 tArg' t12
                        return (tResult, ctx3)

                _ -> throw $ ErrorAppNotFun aApp xx tFn


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
                , indent 2 $ ppr ctx3
                , empty ]

        returnX aApp
                (\z -> XApp z xFn' (XType aArg' tArg'))
                tResult effsFn (closFn `Set.union` t2_clo)
                ctx3


checkAppT !table !ctx0 xx@(XApp aApp _ (XType _ _)) (Check tExpected)
 =      checkSub table aApp ctx0 xx tExpected

checkAppT _ _ _ _
 = error "ddc-core.checkAppT: no match"

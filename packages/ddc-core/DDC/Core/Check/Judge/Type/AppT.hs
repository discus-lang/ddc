
module DDC.Core.Check.Judge.Type.AppT
        (checkAppT)
where
import DDC.Core.Check.Judge.Type.Base
import qualified Data.Set       as Set


-------------------------------------------------------------------------------
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
checkAppT !table !ctx0 xx@(XApp a1 xFn (XType a2 tArg)) mode
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table

        -- Check the functional expression.
        --  TODO: Allow synthesis in the functional expression.
        --        Maybe we want to refactor this type application case to behave
        --        more like the value application case, and have a separate
        --        synthAPPArg function to handle type applications.
        (xFn', tFn, effsFn, closFn, ctx1) 
         <- tableCheckExp table table ctx0 xFn Recon

        -- Check the type argument.
        -- If it's a hole then create a new existential for it.
        (tArg', kArg, ctx2)       
         <- case tArg of
                TVar (UPrim n kArg)
                 |  Just isHole <- configNameIsHole config 
                 ,  isHole n
                 -> do  i2        <- newExists kArg
                        let tArg' = typeOfExists i2
                        let ctx2  = pushExists i2 ctx1
                        return  (tArg', kArg, ctx2)
                _
                 -> do  (tArg', kArg, _)                                        -- TODO: ctx
                                <- checkTypeM config kenv ctx1 UniverseSpec tArg Recon
                        return  (tArg', kArg, ctx1)

        -- Take any Use annots from a region arg.
        --   This always matches because we just checked 't2'
        let Just t2_clo = taggedClosureOfTyArg kenv ctx2 tArg'

        -- Determine the type of the result.
        --  The type of the function must have an outer forall quantifier,
        --  and we instantiate the quantified with the type argument.
        tSynth
         <- case tFn of
                TForall b11 t12
                 | typeOfBind b11 == kArg
                 -> return $ substituteT b11 tArg' t12

                 | otherwise
                 ->  throw $ ErrorAppMismatch a1 xx (typeOfBind b11) tArg'

                _ -> throw $ ErrorAppNotFun   a1 xx tFn

        -- Build an annotated version of the type application.
        let aFn    = AnTEC tSynth (TSum effsFn)  (closureOfTaggedSet closFn) a1  
        let aArg   = AnTEC kArg   (tBot kEffect) (tBot kClosure) a2
        let xx2    = XApp aFn xFn' (XType aArg tArg')

        -- If we have an expected type then force the synthesised type
        -- to be a subtype of it.
        (xx3, tResult, ctx3)
         <- case mode of
                Recon   -> return (xx2, tSynth, ctx2)
                Synth   -> return (xx2, tSynth, ctx2)
                Check tExpect
                 -> do  let tSynth'     = applyContext ctx2 tSynth
                        let tExpect'    = applyContext ctx2 tExpect
                        (xx3, ctx3)     <- makeSub a1 (ErrorMismatch a1 tExpect tSynth xx) 
                                                ctx2 xx2 tSynth' tExpect'
                        return (xx3, tExpect', ctx3)

        ctrace  $ vcat
                [ text "* APP"
                , text "  mode:    " <+> ppr mode
                , text "  inX:     " <+> ppr xx
                , text "  outX:    " <+> ppr xx3
                , text "  tResult: " <+> ppr tResult
                , indent 2 $ ppr ctx3
                , empty ]

        returnX a1 
                (\z -> XApp z xFn' (XType aArg tArg'))
                tResult
                effsFn
                (closFn `Set.union` t2_clo)
                ctx3

checkAppT _ _ _ _
 = error "ddc-core.checkAppT: no match"

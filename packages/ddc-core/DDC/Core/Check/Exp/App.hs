
module DDC.Core.Check.Exp.App
        (checkApp)
where
import DDC.Core.Transform.Reannotate
import DDC.Core.Check.Exp.Base
import qualified DDC.Type.Sum   as Sum
import qualified Data.Set       as Set


checkApp :: Checker a n

-- value-type application -----------------------
--
-- Note: We don't need to substitute into the effect of x1 (effs1)
--       because the body of a type abstraction is required to be pure.
-- 
--       We don't need to substitute into the closure either, because
--       the bound type variable is not visible outside the abstraction.
--       thus we can't be sharing objects that have it in its type.
--
checkApp !table !ctx xx@(XApp a x1 (XType _ t2)) _
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table

        -- Check the functional expression.
        (x1', t1, effs1, clos1, ctx1) 
         <- tableCheckExp table table ctx x1 Recon

        -- Check the type argument.
        (_, k2)         <- checkTypeM config kenv ctx1 t2

        -- Take any Use annots from a region arg.
        --   This always matches because we just checked 't2'
        let Just t2_clo = taggedClosureOfTyArg kenv ctx1 t2

        -- The type of the function must have an outer forall quantifier.
        case t1 of
         TForall b11 t12
          | typeOfBind b11 == k2
          -> returnX a
                (\z -> XApp z x1' (XType z t2))
                (substituteT b11 t2 t12)
                effs1   
                (clos1 `Set.union` t2_clo)
                ctx1

          | otherwise   -> throw $ ErrorAppMismatch a xx (typeOfBind b11) t2
         _              -> throw $ ErrorAppNotFun   a xx t1 t2


-- value-witness application --------------------
checkApp !table !ctx xx@(XApp a x1 (XWitness _ w2)) _
 = do   let config      = tableConfig table
        let kenv        = tableKindEnv table
        let tenv        = tableTypeEnv table

        -- Check the functional expression.
        (x1', t1, effs1, clos1, ctx1) 
         <- tableCheckExp table table ctx x1 Recon

        -- Check the witness.
        (w2', t2)       <- checkWitnessM config kenv tenv ctx1 w2
        let w2TEC = reannotate fromAnT w2'

        -- The type of the function must have an outer implication.
        case t1 of
         TApp (TApp (TCon (TyConWitness TwConImpl)) t11) t12
          | t11 `equivT` t2   
          -> returnX a
                (\z -> XApp z x1' (XWitness z w2TEC))
                t12 effs1 clos1 ctx1

          | otherwise   -> throw $ ErrorAppMismatch a xx t11 t2
         _              -> throw $ ErrorAppNotFun   a xx t1 t2
                 

-- value-value application ----------------------
checkApp !table !ctx xx@(XApp a x1 x2) _
 = do   
        -- Check the functional expression.
        (x1', t1, effs1, clos1, ctx1) 
         <- tableCheckExp table table ctx  x1 Recon

        -- Check the argument.
        (x2', t2, effs2, clos2, ctx2) 
         <- tableCheckExp table table ctx1 x2 Recon

        case t1 of
         -- Oblivious application of a pure function.
         -- Computation of the function and argument may themselves have
         -- an effect, but the function application does not.
         TApp (TApp (TCon (TyConSpec TcConFun)) t11) t12
          | t11 `equivT` t2
          -> returnX a
                (\z -> XApp z x1' x2')
                t12
                (effs1 `Sum.union` effs2)
                (clos1 `Set.union` clos2)
                ctx2

         -- Function with latent effect and closure.
         -- Note: we don't need to use the closure of the function because
         --       all of its components will already be part of clos1 above.
         TApp (TApp (TApp (TApp (TCon (TyConSpec TcConFunEC)) t11) eff) _clo) t12
          | t11 `equivT` t2   
          , effs    <- Sum.fromList kEffect  [eff]
          -> returnX a
                (\z -> XApp z x1' x2')
                t12
                (effs1 `Sum.union` effs2 `Sum.union` effs)
                (clos1 `Set.union` clos2)
                ctx2

          | otherwise   -> throw $ ErrorAppMismatch a xx t11 t2
         _              -> throw $ ErrorAppNotFun a xx t1 t2

-- others ---------------------------------------
checkApp _ _ _ _
 = error "ddc-core.checkApp: no match"


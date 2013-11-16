
module DDC.Core.Check.Exp.App
        (checkApp)
where
import DDC.Core.Check.Exp.Base
import DDC.Core.Check.Exp.Inst
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
         _              -> throw $ ErrorAppNotFun   a xx t1


-- value-value application ----------------------
checkApp !table !ctx xx@(XApp a x1 x2) Recon
 = do   
        -- Check the functional expression.
        (x1', t1, effs1, clos1, ctx1) 
         <- tableCheckExp table table ctx  x1 Recon

        -- Check the argument.
        (x2', t2, effs2, clos2, ctx2) 
         <- tableCheckExp table table ctx1 x2 Recon

        applyFunctionType
                a xx
                x1' t1 effs1 clos1
                x2' t2 effs2 clos2
                ctx2

-- Rule (-> Elim)
checkApp !table !ctx0 xx@(XApp a x1 x2) Synth
 = do   
        -- Synth a type for the functional expression.
        (x1', t1, effs1, clos1, ctx1) 
         <- tableCheckExp table table ctx0  x1 Synth

        -- Substitute context into synthesised type.
        let t1' = applyContext ctx1 t1

        -- Synth a type for the function applied to its argument.
        synthAppArg table a xx
                ctx1
                x1' t1' effs1 clos1 
                x2

checkApp !table !ctx xx@(XApp a _ _) (Check tEx)
 =      checkSub table a ctx xx tEx


-- others ---------------------------------------
checkApp _ _ _ _
 = error "ddc-core.checkApp: no match"


-------------------------------------------------------------------------------
-- | Synthesise the type of a function applied to its argument.
synthAppArg 
        :: (Show n, Ord n, Pretty n)
        => Table a n
        -> a                             -- Annot of original expression (for error msgs)
        -> Exp a n                       -- Original expression (for error msgs)
        -> Context n                     -- Current context.
        -> Exp (AnTEC a n) n             -- Checked functional expression.
                -> Type n                -- Type of functional expression.
                -> TypeSum n             -- Effect of functional expression.
                -> Set (TaggedClosure n) -- Closure of functional expression.
        -> Exp a n                       -- Function argument.
        -> CheckM a n
                ( Exp (AnTEC a n) n
                , Type n
                , TypeSum n
                , Set (TaggedClosure n)
                , Context n)

synthAppArg table a xx ctx0 xF tF effsF closF xArg 

 -- Rule (^alpha App)
 --  Functional type is an existential.
 | Just iF     <- takeExists tF
 = do   
        -- Make new existentials to match the function type and parameter.
        iA1     <- newExists
        let tA1 = typeOfExists iA1

        iA2     <- newExists
        let tA2 = typeOfExists iA2

        -- Update the context with the new constraint.
        let ctx1 = updateExists [iA2, iA1] iF (tFun tA1 tA2) ctx0

        -- Check the argument under the new context.
        (xArg', _, effsA, closA, ctx2)
         <- tableCheckExp table table ctx1 xArg (Check tA1)

        let effsFA = effsF `Sum.union` effsA
        let closFA = closF `Set.union` closA

        ctrace  $ vcat
                [ text "* App Synth (^alpha)"
                , indent 2 $ ppr xx
                , indent 2 $ ppr ctx2 ]

        returnX a
           (\z -> XApp z xF xArg')
           tA2 effsFA closFA ctx2

 
 -- Rule (-> App)
 --  Function already has a concrete function type.
 | Just (t11, _t12)   <- takeTFun tF
 = do   
        -- Check the argument.
        (xArg', tArg, effsArg, closArg, ctx1) 
         <- tableCheckExp table table ctx0 xArg (Check t11)

        ctrace  $ vcat
                [ text "* App Synth Elim"
                , indent 2 $ ppr xx
                , indent 2 $ ppr ctx1 ]

        applyFunctionType
                a xx
                xF    tF   effsF   closF
                xArg' tArg effsArg closArg
                ctx1

 | otherwise
 = throw $ ErrorAppNotFun a xx tF


-- apply ----------------------------------------------------------------------
-- Get the result type of an applied function.
applyFunctionType 
        :: Ord n
        => a                  -> Exp a n
        -> Exp (AnTEC a n) n  -> Type n -> TypeSum n -> Set (TaggedClosure n)
        -> Exp (AnTEC a n) n  -> Type n -> TypeSum n -> Set (TaggedClosure n)
        -> Context n
        -> CheckM a n 
                ( Exp (AnTEC a n) n
                , Type n
                , TypeSum n
                , Set (TaggedClosure n)
                , Context n)

applyFunctionType a xx
        x1 t1 effs1 clos1 
        x2 t2 effs2 clos2
        ctx
 = let  effs12     = effs1 `Sum.union` effs2
        clos12     = clos1 `Set.union` clos2
   in  case t1 of
        -- Discharge an implication.
         TApp (TApp (TCon (TyConWitness TwConImpl)) t11) t12
          | t11 `equivT` t2
          -> returnX a 
                (\z -> XApp z x1 x2)
                t12 effs12 clos12 ctx

         -- Oblivious application of a pure function.
         -- Computation of the function and argument may themselves have
         -- an effect, but the function application does not.
         TApp (TApp (TCon (TyConSpec TcConFun)) t11) t12
          | t11 `equivT` t2
          -> returnX a
                (\z -> XApp z x1 x2)
                t12 effs12 clos12 ctx

         -- Function with latent effect and closure.
         -- Note: we don't need to use the closure of the function because
         --       all of its components will already be part of clos1 above.
         TApp (TApp (TApp (TApp (TCon (TyConSpec TcConFunEC)) t11) eff) _clo) t12
          | t11 `equivT` t2   
          , effs    <- Sum.fromList kEffect  [eff]
          -> returnX a
                (\z -> XApp z x1 x2)
                t12 (effs12 `Sum.union` effs) clos12 ctx

          | otherwise   -> throw $ ErrorAppMismatch a xx t11 t2
         _              -> throw $ ErrorAppNotFun   a xx t1



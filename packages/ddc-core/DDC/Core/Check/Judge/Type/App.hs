
module DDC.Core.Check.Judge.Type.App
        (checkApp)
where
import DDC.Core.Check.Judge.Type.Sub
import DDC.Core.Check.Judge.Type.Base
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
checkApp !table !ctx xx@(XApp a xFn xArg) Recon
 = do   
        -- Check the functional expression.
        (xFn',  tFn,  effsFn,  closFn,  ctx1) 
         <- tableCheckExp table table ctx  xFn Recon

        -- Check the argument.
        (xArg', tArg, effsArg, closArg, ctx2) 
         <- tableCheckExp table table ctx1 xArg Recon

        (tResult, effsLatent)
         <- case splitFunType tFn of
             Just (tParam, effs, _, tResult)
              | tParam `equivT` tArg 
              -> return (tResult, effs)

              | otherwise           
              -> throw  $ ErrorAppMismatch a xx tParam tArg

             Nothing
              -> throw  $ ErrorAppNotFun a xx tFn

        let effsResult  = Sum.unions kEffect
                        $ [effsFn, effsArg, Sum.singleton kEffect effsLatent]
        let closResult  = Set.union  closFn closArg

        returnX a 
                (\z -> XApp z xFn' xArg')
                tResult effsResult closResult ctx2

-- Rule (-> Elim)
checkApp !table !ctx0 xx@(XApp a xFn xArg) Synth
 = do   
        -- Synth a type for the functional expression.
        (xFn', tFn, effsFn, closFn, ctx1) 
         <- tableCheckExp table table ctx0 xFn Synth

        -- Substitute context into synthesised type.
        let tFn' = applyContext ctx1 tFn

        -- Synth a type for the function applied to its argument.
        (xFn'', xArg', tResult, effsResult, closResult, ctx2)
         <- synthAppArg table a xx ctx1
                xFn' tFn' effsFn closFn 
                xArg

        ctrace  $ vcat
                [ text "* App Synth"
                , indent 2 $ ppr xx
                , text "  TFUN:  " <> ppr tFn'
                , text "   ARG:  " <> ppr xArg
                , text "  TYPE:  " <> ppr tResult
                , ppr ctx0
                , ppr ctx2
                , empty ]

        returnX a 
                (\z -> XApp z xFn'' xArg')
                tResult effsResult closResult ctx2


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
        -> a                             -- Annot for error messages.
        -> Exp a n                       -- Expression for error messages.
        -> Context n                     -- Current context.
        -> Exp (AnTEC a n) n             -- Checked functional expression.
                -> Type n                -- Type of functional expression.
                -> TypeSum n             -- Effect of functional expression.
                -> Set (TaggedClosure n) -- Closure of functional expression.
        -> Exp a n                       -- Function argument.
        -> CheckM a n
                ( Exp (AnTEC a n) n      -- Checked functional expression.
                , Exp (AnTEC a n) n      -- Checked argument   expression.
                , Type n                 -- Type of result.
                , TypeSum n              -- Effect of result.
                , Set (TaggedClosure n)  -- Closure of result.
                , Context n)             -- Result context.

synthAppArg table a xx ctx0 xFn tFn effsFn closFn xArg

 -- Rule (App Synth exists)
 --  Functional type is an existential.
 | Just iFn      <- takeExists tFn
 = do   
        -- New existential for the type of the function parameter.
        iA1      <- newExists kData
        let tA1  = typeOfExists iA1

        -- New existential for the type of the function result.
        iA2      <- newExists kData
        let tA2  = typeOfExists iA2

        -- Update the context with the new constraint.
        let ctx1 = updateExists [iA2, iA1] iFn (tFun tA1 tA2) ctx0

        -- Check the argument under the new context.
        (xArg', _, effsArg, closArg, ctx2)
         <- tableCheckExp table table ctx1 xArg (Check tA1)

        -- Effect and closure of the overall function application.
        let effsResult = effsFn `Sum.union` effsArg
        let closResult = closFn `Set.union` closArg

        ctrace  $ vcat
                [ text "* App Synth exists"
                , indent 2 $ ppr xx
                , indent 2 $ ppr ctx2 
                , empty ]

        return  ( xFn, xArg'
                , tA2, effsResult, closResult, ctx2)


 -- Rule (App Synth Forall)
 --  Function has a quantified type, but we're applying an expression to it.
 --  We need to inject a new type argument.
 | TForall b tBody      <- tFn
 = do   
        -- Make a new existential for the type of the argument,
        -- and push it onto the context.
        iA         <- newExists (typeOfBind b)
        let tA     = typeOfExists iA
        let ctx1   = pushExists iA ctx0

        -- Instantiate the type of the function with the new existential.
        let tBody' = substituteT b tA tBody

        -- Add the missing type application.
        --  Because we were applying a function to an expression argument, 
        --  and the type of the function was quantified, we know there should
        --  be a type application here.
        let aFn    = AnTEC tFn (TSum effsFn) (closureOfTaggedSet closFn) a
        let aArg   = AnTEC (typeOfBind b) (tBot kEffect) (tBot kClosure) a
        let xFnTy  = XApp aFn xFn (XType aArg tA)

        -- Synthesise the result type of a function being applied to its 
        -- argument. We know the type of the function up-front, but we pass
        -- in the whole argument expression.
        (  xFnTy', xArg', tResult, effsResult, closResult, ctx2)
         <- synthAppArg table a xx ctx1 xFnTy tBody' effsFn closFn xArg

        ctrace  $ vcat
                [ text "* App Synth Forall"
                , text "      xFn:  " <> ppr xFnTy'
                , text "     tArg:  " <> ppr xArg'
                , text "      tFn:  " <> ppr tFn
                , text "  tResult:  " <> ppr tResult
                , indent 2 $ ppr ctx2
                , empty ]

        return  ( xFnTy'
                , xArg'
                , tResult, effsResult, closResult, ctx2)


 -- Rule (App Synth Fun)
 --  Function already has a concrete function type.
 | Just (tParam, tResult)   <- takeTFun tFn
 = do   
        -- Check the argument.
        (xArg', tArg, effsArg, closArg, ctx1) 
         <- tableCheckExp table table ctx0 xArg (Check tParam)

        let tFn1     = applyContext ctx1 tFn
        let tArg1    = applyContext ctx1 tArg
        let tResult1 = applyContext ctx1 tResult

        -- Get the type, effect and closure resulting from the application
        -- of a function of this type to its argument.
        effsLatent
         <- case splitFunType tFn1 of
             Just (_tParam, effsLatent, _closLatent, _tResult)
              -> return effsLatent

             Nothing
              -> throw  $ ErrorAppNotFun a xx tFn1

        let effsResult  = Sum.unions kEffect
                        $ [ effsFn, effsArg, Sum.singleton kEffect effsLatent]
        
        let closResult  = Set.union closFn closArg

        ctrace  $ vcat
                [ text "* App Synth Fun"
                , indent 2 $ ppr xx
                , text "      tFn: " <> ppr tFn1
                , text "     tArg: " <> ppr tArg1
                , text "  tResult: " <> ppr tResult1
                , indent 2 $ ppr ctx1
                , empty ]

        return  ( xFn, xArg'
                , tResult, effsResult, closResult, ctx1)

 | otherwise
 = throw $ ErrorAppNotFun a xx tFn


-------------------------------------------------------------------------------
splitFunType 
        :: Type n
        -> Maybe (Type n, Effect n, Closure n, Type n)

splitFunType tt
 = case tt of
        TApp (TApp (TCon (TyConWitness TwConImpl)) t11) t12
          -> Just (t11, tBot kEffect, tBot kClosure, t12)

        TApp (TApp (TCon (TyConSpec TcConFun)) t11) t12
          -> Just (t11, tBot kEffect, tBot kClosure, t12)

        TApp (TApp (TApp (TApp (TCon (TyConSpec TcConFunEC)) t11) eff) clo) t12
          -> Just (t11, eff, clo, t12)

        _ -> Nothing
         

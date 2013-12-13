
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
checkApp !table !ctx xx@(XApp a x1 x2) Recon
 = do   
        -- Check the functional expression.
        (x1', t1, effs1, clos1, ctx1) 
         <- tableCheckExp table table ctx  x1 Recon

        -- Check the argument.
        (x2', t2, effs2, clos2, ctx2) 
         <- tableCheckExp table table ctx1 x2 Recon

        (tResult, effsResult, closResult)
         <- applyFunctionType
                a xx
                t1 effs1 clos1
                t2 effs2 clos2

        returnX a 
                (\z -> XApp z x1' x2')
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
        iA1      <- newExists
        let tA1  = typeOfExists iA1

        -- New existential for the type of the function result.
        iA2      <- newExists
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
        iA         <- newExists
        let tA     = typeOfExists iA
        let ctx1   = pushExists iA ctx0

        -- Instantiate the type of the function with the new existential.
        let tBody' = substituteT b tA tBody

        -- Add the missing type application.
        --  Because we were applying a function to an expression argument, 
        --  and the type of the function was quantified, we know there should
        --  be a type application here.
        let aFn    = AnTEC tFn (TSum effsFn) (closureOfTaggedSet closFn) a
        let aArg   = AnTEC kData (tBot kEffect) (tBot kClosure) a
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

        -- Get the type, effect and closure resulting from the application
        -- of a function of this type to its argument.
        (_, effsResult, closResult)
         <- applyFunctionType
                a xx
                tFn  effsFn   closFn
                tArg effsArg closArg

        ctrace  $ vcat
                [ text "* App Synth Fun"
                , indent 2 $ ppr xx
                , text "      tFn: " <> ppr tFn
                , text "     tArg: " <> ppr tArg
                , text "  tResult: " <> ppr tResult
                , indent 2 $ ppr ctx1 
                , empty ]

        return  ( xFn, xArg'
                , tResult, effsResult, closResult, ctx1)

 | otherwise
 = throw $ ErrorAppNotFun a xx tFn


-------------------------------------------------------------------------------
-- | Get the result type, effect and closure of an applied function.
--   The way we do this depends on what sort of function constructor we have.
applyFunctionType 
        :: Ord n
        => a            -- Annotation, for error messages.
        -> Exp a n      -- Original expression, for error messages.
        -> Type n -> TypeSum n -> Set (TaggedClosure n)
        -> Type n -> TypeSum n -> Set (TaggedClosure n)
        -> CheckM a n 
                ( Type n
                , TypeSum n
                , Set (TaggedClosure n))

applyFunctionType a xx
        tFn  effsFn  closFn
        tArg effsArg closArg
 = let  effsResult      = effsFn `Sum.union` effsArg
        closResult      = closFn `Set.union` closArg
   in  case tFn of
        -- Discharge an implication.
         TApp (TApp (TCon (TyConWitness TwConImpl)) t11) t12
          | t11 `equivT` tArg
          -> return (t12, effsResult, closResult)

         -- Oblivious application of a pure function.
         -- Computation of the function and argument may themselves have
         -- an effect, but the function application does not.
         TApp (TApp (TCon (TyConSpec TcConFun)) t11) t12
          | t11 `equivT` tArg
          -> return (t12, effsResult, closResult)

         -- Function with latent effect and closure.
         -- Note: we don't need to use the closure of the function because
         --       all of its components will already be part of clos1 above.
         TApp (TApp (TApp (TApp (TCon (TyConSpec TcConFunEC)) t11) eff) _clo) t12
          | t11 `equivT` tArg   
          , effs    <- Sum.fromList kEffect [eff]
          -> return (t12, effsResult `Sum.union` effs, closResult)

          | otherwise   -> throw $ ErrorAppMismatch a xx t11 tArg
         _              -> throw $ ErrorAppNotFun   a xx tFn


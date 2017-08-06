
module DDC.Core.Check.Judge.Sub
        (makeSub)
where
import DDC.Type.Transform.SubstituteT
import DDC.Core.Check.Judge.EqT
import DDC.Core.Exp.Annot.AnTEC
import DDC.Core.Check.Judge.Inst
import DDC.Core.Check.Base
import qualified DDC.Core.Check.Context as Context
import qualified DDC.Core.Env.EnvT      as EnvT
import qualified Data.Map.Strict        as Map
import qualified DDC.Type.Sum           as Sum


-- | Make the left type a subtype of the right type,
--   or throw the provided error if this is not possible.
--
--   The inferred type may already be a subtype of the expected type,
--   and in that case we don't need to do anything extra.
--
--   If the inferred type is a 'S e a' computation type and the expected
--   type is 'a' then we can force the inferred type to be the expected one
--   by running the computation. In this case we end up with more effects.
--
makeSub :: (Eq n, Ord n, Show n, Pretty n)
        => Config n                     -- ^ Type checker configuration.
        -> a                            -- ^ Current annotation.
        -> Context n                    -- ^ Input context.
        -> Exp a n                      -- ^ Original expression, for error reporting.
        -> Exp  (AnTEC a n) n           -- ^ Expression that we've inferred the type of.
        -> Type n                       -- ^ Inferred type of the expression.
        -> Type n                       -- ^ Expected type of the expression.
        -> Error a n                    -- ^ Error to throw if we can't force subsumption.
        -> CheckM a n
                ( Exp (AnTEC a n) n     --   Expression after instantiations and running.
                , TypeSum n             --   More effects we might get from running the computation.
                , Context n)            --   Output context.

-- NOTE: The order of cases matters here.
--       For example, we do something different when both sides are
--       existentials, vs the case when only one side is an existential.
makeSub config a ctx0 x0 xL tL tR err

 -- Sub_SynL
 --   Expand type synonym on the left.
 | TCon (TyConBound (UName n) _) <- tL
 , Just tL'  <- Map.lookup n $ EnvT.envtEquations
                             $ Context.contextEnvT ctx0
 = do
        ctrace  $ vcat
                [ text "**  Sub_SynL"
                , text "    tL:  " <> ppr tL
                , text "    tL': " <> ppr tL'
                , text "    tR:  " <> ppr tR
                , empty ]

        makeSub config a ctx0 x0 xL tL' tR err


 -- Sub_SynR
 --   Expand type synonym on the right.
 | TCon (TyConBound (UName n) _) <- tR
 , Just tR'  <- Map.lookup n $ EnvT.envtEquations
                             $ Context.contextEnvT ctx0
 = do
        ctrace  $ vcat
                [ text "**  Sub_SynR"
                , text "    tL:  " <> ppr tL
                , text "    tR:  " <> ppr tR
                , text "    tR': " <> ppr tR
                , empty ]

        makeSub config a ctx0 x0 xL tL tR' err


 -- Sub_ExVar
 --   Both sides are already the same existential,
 --   so we don't need to do anything further.
 | Just iL <- takeExists tL
 , Just iR <- takeExists tR
 , iL == iR
 = do
        ctrace  $ vcat
                [ text "**  Sub_ExVar"
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , text "    xL: " <> ppr xL
                , indent 4 $ ppr ctx0
                , empty ]

        return  ( xL
                , Sum.empty kEffect
                , ctx0)


 -- SubInstL
 --   Left is an existential.
 | Just iL      <- takeExists tL
 = do   ctx1    <- makeInstL config a ctx0 iL tR err

        ctrace  $ vcat
                [ text "**  Sub_InstL"
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , text "    xL: " <> ppr xL
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return  ( xL
                , Sum.empty kEffect
                , ctx1)


 -- SubInstR
 --   Right is an existential.
 | Just iR      <- takeExists tR
 = do   ctx1    <- makeInstR config a ctx0 tL iR err

        ctrace  $ vcat
                [ text "**  Sub_InstR"
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , text "    xL: " <> ppr xL
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , empty ]

        return  ( xL
                , Sum.empty kEffect
                , ctx1)


 -- Sub_Con
 --   Both sides are type constructors which are equivalent.
 --
 --   ISSUE #378: Complete merging (~>) and (->) type constructors.
 --   The equivTyCon function already treats these equivalent,
 --   but we should just use (->) at all levels and ditch (~>).
 --
 | TCon tc1     <- tL
 , TCon tc2     <- tR
 , equivTyCon tc1 tc2
 = do
        -- Only trace rule if it's done something interesting.
        when (not $ tc1 == tc2)
         $ ctrace  $ vcat
                [ text "**  Sub_Con"
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , text "    xL: " <> ppr xL
                , empty ]

        return  ( xL
                , Sum.empty kEffect
                , ctx0)


 -- Sub_Var
 --   Both sides are the same (rigid) type variable,
 --   so we don't need to do anything further.
 | TVar u1      <- tL
 , TVar u2      <- tR
 , u1 == u2
 = do
        -- Suppress tracing of noisy rule.
        -- ctrace  $ vcat
        --         [ text "**  Sub_Var"
        --         , text "    tL: " <> ppr tL
        --         , text "    tR: " <> ppr tR
        --         , text "    xL: " <> ppr xL
        --         , empty ]

        return  ( xL
                , Sum.empty kEffect
                , ctx0)

 -- Sub_Equiv
 --   Both sides are equivalent.
 --   The `equivT` function will also crush any effect types,
 --   and handle comparing type sums for equivalence.
 --
 | equivT (contextEnvT ctx0) tL tR
 = do
        ctrace  $ vcat
                [ text "**  Sub_Equiv"
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , text "    xL: " <> ppr xL
                , indent 4 $ ppr ctx0
                , empty ]

        return  ( xL
                , Sum.empty kEffect
                , ctx0)


 -- Sub_Arr
 --   Both sides are arrow types.
 --   Make sure to check the parameter type contravariantly.
 --
 | Just (tL1, tL2)  <- takeTFun tL
 , Just (tR1, tR2)  <- takeTFun tR
 = do
        ctrace  $ vcat
                [ text "*>  Sub_Arr"
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , text "    xL: " <> ppr xL
                , empty ]

        (_, effs1, ctx1) <- makeSub config a ctx0 x0 xL tR1 tL1 err
        tL2'             <- applyContext     ctx1 tL2
        tR2'             <- applyContext     ctx1 tR2
        (_, effs2, ctx2) <- makeSub config a ctx1 x0 xL tL2' tR2' err

        ctrace  $ vcat
                [ text "*<  Sub_Arr"
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , indent 4 $ ppr ctx2
                , empty ]

        return  ( xL
                , Sum.union effs1 effs2
                , ctx2)

 -- Sub_Implicit
 --
 --   The inferred type  is   (A ~> B)
 --   while expected type is  (D t1'..tn')
 --   where D is not (~>) and not a type synonym.
 --
 --   We'll inject a new implicit argument that tries to elaborate a value
 --   of type 'A', then hope that B can be made a subtype of (D t1'..tn').
 --
 | Just (TcConFunImplicit, tL1, tL2)    <- takeTFunCon tL
 , case takeTFunCon tR of
        Just (TcConFunImplicit, _, _)   -> False
        _                               -> True
 = do
        ctrace  $ vcat
                [ text "*>  Sub_Implicit_Right"
                , text "    tL:      " <> ppr tL
                , text "    tR:      " <> ppr tR
                , text "    xL:      " <> ppr xL
                , empty ]

        let tArg        = tL1
        let tElaborate  = tForall kData $ \tA -> tA
        let aArg        = AnTEC tArg       (tBot kEffect) (tBot kClosure) a
        let aFnElab     = AnTEC tElaborate (tBot kEffect) (tBot kClosure) a
        let xArgElab    = XApp aArg (XPrim aFnElab PElaborate) (RType tArg)

        -- ISSUE #431: Sub_Implcit_Right rule assumes functional expression has no effects.
        let aFn         = AnTEC tL    (tBot kEffect) (tBot kClosure) a
        let xL_elab     = XApp aFn xL (RImplicit (RTerm xArgElab))

        (xL_elab', effs1, ctx1)
                <- makeSub config a ctx0 x0 xL_elab tL2 tR err

        ctrace  $ vcat
                [ text "*<  Sub_Implicit_Arg"
                , text "    tL:       " <> ppr tL
                , text "    tR:       " <> ppr tR
                , text "    xL_elab': " <> ppr xL_elab'
                , empty ]

        return  ( xL_elab'
                , effs1
                , ctx1)


 -- Sub_Run
 --   The left (inferred) type is a suspension, but the right it not.
 --   We run the suspension to get the result value and check if the
 --   result has the expected type. Running the suspension causes some more
 --   effects which we pass pack to the caller.
 --
 | Just    (tEffect, tResult)   <- takeTSusp tL
 , Nothing                      <- takeTSusp tR
 = do
        ctrace  $ vcat
                [ text "**  Sub_Run"
                , text "    tL:      " <> ppr tL
                , text "    tR:      " <> ppr tR
                , text "    xL:      " <> ppr xL
                , text "    tEffect: " <> ppr tEffect
                , text "    tResult: " <> ppr tResult
                , empty ]

        let aRun    = AnTEC tResult tEffect (tBot kClosure) a
        let xL_run  = XCast aRun CastRun xL

        (xL2, eff2, ctx2)
         <- makeSub config a ctx0 x0 xL_run tResult tR err

        let eff = Sum.unions    kEffect
                [ Sum.singleton kEffect tEffect
                , eff2 ]

        return  ( xL2
                , eff
                , ctx2)


 -- Sub_App
 --   ISSUE #379: Track variance information in type synonyms.
 --   We're treating all non-function types as invariant, so use makeEqT
 --   rather than checking for subsumption.
 --
 | TApp tL1 tL2 <- tL
 , TApp tR1 tR2 <- tR
 = do
        ctrace  $ vcat
                [ text "*>  Sub_App"
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , text "    xL: " <> ppr xL
                , empty ]

        ctx1    <- makeEqT config ctx0 tL1 tR1 err
        tL2'    <- applyContext ctx1 tL2
        tR2'    <- applyContext ctx1 tR2
        ctx2    <- makeEqT config ctx1 tL2' tR2' err

        ctrace  $ vcat
                [ text "*<  Sub_App"
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , indent 4 $ ppr ctx2
                , empty ]

        return  ( xL
                , Sum.empty kEffect
                , ctx2)


 -- Sub_ForallL
 --   Left (inferred) type is a forall type.
 --   Apply the expression to a new existential to instantiate it,
 --   then check the new instantiated type against the expected one.
 --
 | TForall b t1 <- tL
 = do
        -- Make a new existential to instantiate the quantified
        -- variable and substitute it into the body.
        iA        <- newExists (typeOfBind b)
        let tA    = typeOfExists iA
        let t1'   = substituteT b tA t1

        ctrace  $ vcat
                [ text "*>  Sub_ForallL"
                , text "    tL:  " <> ppr tL
                , text "    tR:  " <> ppr tR
                , text "    xL:  " <> ppr xL
                , text "    iA:  " <> ppr iA
                , text "    t1': " <> ppr t1'
                , empty ]

        -- Check the new body against the right type,
        -- so that the existential we just made is instantiated
        -- to match the right. The new existential is used to constrain the
        -- expected type, so it needs to be in the correct scope.
        let (ctx1, pos1) =  markContext ctx0
        let ctx2         =  pushExistsScope iA (slurpExists tR) ctx1

        -- Wrap the expression with a type application to cause
        -- the instantiation.
        let AnTEC _ e0 c0 _
                 = annotOfExp xL
        let aFn  = AnTEC t1' (substituteT b tA e0) (substituteT b tA c0) a
        let xL1  = XApp aFn xL (RType tA)

        (xL2, effs3, ctx3)
         <- makeSub config a ctx2 x0 xL1 t1' tR err

        -- Pop the existential and constraints above it back off
        -- the stack.
        let ctx4  = popToPos pos1 ctx3

        ctrace  $ vcat
                [ text "*<  Sub_ForallL"
                , text "    tL:  " <> ppr tL
                , text "    tR:  " <> ppr tR
                , text "    xL:  " <> ppr xL
                , text "    xL2: " <> ppr xL2
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx4
                , empty ]

        return  ( xL2
                , effs3
                , ctx4)


 -- Sub_ForallR
 --   The right (expected) type is a forall type.
 --
 | TForall bParamR tBodyR  <- tR
 = do
        ctrace  $ vcat
                [ text "*>  Sub_ForallR"
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , text "    xL: " <> ppr xL
                , empty ]

        -- Make a new existential to instantiate the quantified
        -- variable and substitute it into the body.
        let Just uParam = takeSubstBoundOfBind bParamR
        let tA          = TVar uParam
        let tBodyR'     = substituteT bParamR tA tBodyR

        -- Check the new body against the left type,
        -- so that the existential is instantiated
        -- to match the left.
        let (ctx1, pos1)  =  markContext ctx0
        let ctx2          =  pushKind bParamR RoleConcrete ctx1

        (xL2, eff2, ctx3) <- makeSub config a ctx2 x0 xL tL tBodyR' err

        -- The body of our new type abstraction must be pure.
        when (not $ eff2 == Sum.empty kEffect)
         $ throw $ ErrorAbsNotPure a x0 UniverseSpec (TSum eff2)

        tBodyR_ctx3       <- applyContext ctx3 tBodyR'
        let tR'           =  TForall bParamR tBodyR_ctx3
        let aApp          =  AnTEC tR' (tBot kEffect) (tBot kClosure) a
        let xL_abs        =  XLAM aApp bParamR xL2

        -- Pop the existential and constraints above it off the stack.
        let ctx4        = popToPos pos1 ctx3

        ctrace  $ vcat
                [ text "*<  SubForallR"
                , text "    tL:     " <> ppr tL
                , text "    tR:     " <> ppr tR
                , text "    xL:     " <> ppr xL
                , text "    xL_abs: " <> ppr xL_abs
                , empty ]

        return  ( xL_abs
                , Sum.empty kEffect
                , ctx4)


 -- Sub_Fail
 --   No other rule matched, so this expression is ill-typed.
 | otherwise
 = do
        ctrace  $ vcat
                [ text "**  Sub_Fail"
                , text "    tL: " <> ppr tL
                , text "    tR: " <> ppr tR
                , empty ]

        throw err


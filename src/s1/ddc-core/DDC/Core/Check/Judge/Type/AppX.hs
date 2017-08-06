{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Judge.Type.AppX
        (checkAppX)
where
import DDC.Core.Check.Judge.Type.Sub
import DDC.Core.Check.Judge.Type.Prim
import DDC.Core.Check.Judge.Type.Base
import qualified DDC.Core.Env.EnvT      as EnvT
import qualified DDC.Type.Sum           as Sum
import qualified Data.Map.Strict        as Map


-- | Check a value expression application.
checkAppX :: Checker a n

checkAppX !table !ctx
        Recon demand
        xx@(XApp a xFn arg)
 = do
        -- Check the functional expression.
        (xFn',  tFn,  effsFn, ctx1)
         <- tableCheckExp table table ctx  Recon demand xFn

        -- Check the argument.
        (arg', tArg, effsArg, ctx2)
         <- checkArg table ctx1 Recon DemandNone arg

        -- The type of the parameter must match that of the argument.
        (tResult, effsLatent)
         <- case splitFunType tFn of
             Just (tParam, effs, _, tResult)
              |  equivT (contextEnvT ctx2) tParam tArg
              -> return (tResult, effs)

              | otherwise
              -> throw  $ ErrorMismatch a tParam tArg xx

             Nothing
              -> throw  $ ErrorAppNotFun a xx tFn

        -- Effect of the overall application.
        let effsResult
                = Sum.unions kEffect
                $ [effsFn, effsArg, Sum.singleton kEffect effsLatent]

        returnX a
                (\z -> XApp z xFn' arg')
                tResult effsResult
                ctx2


checkAppX !table !ctx0
        mode@(Synth isScope)
        demand
        xx@(XApp a xFn arg)
 = do
        ctrace  $ vcat
                [ text "*>  App Synth"
                , text "    mode    = " <> ppr mode
                , text "    xx      = " <> ppr xx
                , empty ]

        -- Synth a type for the functional expression.
        (xFn', tFn, effsFn, ctx1)
         <- tableCheckExp table table ctx0 mode demand xFn

        -- Substitute context into synthesised type.
        tFn' <- applyContext ctx1 tFn

        -- Synth a type for the function applied to its argument.
        (xResult, tResult, esResult, ctx2)
         <- synthAppArg table a xx
                ctx1 demand isScope
                xFn' tFn' effsFn arg

        ctrace  $ vcat
                [ text "*<  App Synth"
                , text "    mode    = " <> ppr mode
                , text "    demand  = " <> (text $ show demand)
                , indent 4 $ ppr xx
                , text "    tFn     = " <> ppr tFn'
                , text "    arg     = " <> ppr arg
                , text "    xResult = " <> ppr xResult
                , text "    tResult = " <> ppr tResult
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx2
                , empty ]

        return  (xResult, tResult, esResult, ctx2)


checkAppX !table !ctx
        (Check tExpected) demand
        xx@(XApp a _ _)
 = do
        ctrace  $ vcat
                [ text "*>  App Check"
                , text "    tExpected = " <> ppr tExpected
                , empty ]

        result  <- checkSub table a ctx demand xx tExpected

        ctrace  $ vcat
                [ text "*<  App Check"
                , empty ]

        return  result

checkAppX _ _ _ _ _
 = error "ddc-core.checkApp: no match"


checkArg  table ctx mode demand arg
 = case arg of
        RTerm x
          -> do (x', t', effs', ctx')
                 <- tableCheckExp table table ctx mode demand x
                return (RTerm x',     t', effs', ctx')

        RImplicit (RTerm x)
          -> do (x', t', effs', ctx')
                 <- tableCheckExp table table ctx mode demand x
                return (RImplicit (RTerm x'), t', effs', ctx')

        _ -> error "checkArg: nope"



-------------------------------------------------------------------------------
-- | Synthesize the type of a function applied to its argument.
synthAppArg
        :: (Show a, Show n, Ord n, Pretty n)
        => Table a n
        -> a                         -- Annot for error messages.
        -> Exp a n                   -- Expression for error messages.
        -> Context n                 -- Current context.
        -> Demand                    -- Demand placed on result of application.
        -> [Exists n]                -- Scope for new unification variables.
        -> Exp (AnTEC a n) n         -- Checked functional expression.
                -> Type n            -- Type of functional expression.
                -> TypeSum n         -- Effect of functional expression.
        -> Arg a n                   -- Function argument.
        -> CheckM a n
                ( Exp (AnTEC a n) n  -- Checked application.
                , Type n             -- Type of result.
                , TypeSum n          -- Effect of result.
                , Context n)         -- Result context.

synthAppArg table
        a xx ctx0
        demand isScope
        xFn tFn effsFn arg

 -- Look through synonyms in the functional type
 | TCon (TyConBound (UName n) _) <- tFn
 , Just tFn'    <- Map.lookup n $ EnvT.envtEquations $ contextEnvT ctx0
 = do   synthAppArg table a xx ctx0 demand isScope xFn tFn' effsFn arg

 -- Rule (App Synth exists)
 --  Functional type is an existential.
 | Just iFn     <- takeExists tFn
 = do
        ctrace  $ vcat
                [ text "*>  App Synth Exists"
                , text "    demand = " <> ppr demand
                , text "    scope  = " <> ppr isScope
                , empty ]

        -- New existential for the type of the function parameter.
        iA1      <- newExists kData
        let tA1  = typeOfExists iA1

        -- New existential for the type of the function result.
        iA2      <- newExists kData
        let tA2  = typeOfExists iA2

        -- Update the context with the new constraint.
        let Just ctx1 = updateExists [iA2, iA1] iFn (tFun tA1 tA2) ctx0

        -- Check the argument under the new context.
        (arg', _, effsArg, ctx2)
         <- checkArg table ctx1 (Check tA1) DemandRun arg

        -- Effect and closure of the overall function application.
        let esResult    = effsFn `Sum.union` effsArg

        -- Result expression.
        let xResult    = XApp   (AnTEC tA2 (TSum esResult) (tBot kClosure) a)
                                xFn arg'

        ctrace  $ vcat
                [ text "*<  App Synth Exists"
                , text "    xFn     ="  <> ppr xFn
                , text "    tFn     ="  <> ppr tFn
                , text "    arg     ="  <> ppr arg
                , text "    arg'    ="  <> ppr arg'
                , text "    xResult ="  <> ppr xResult
                , indent 4 $ ppr xx
                , indent 4 $ ppr ctx2
                , empty ]

        return  (xResult, tA2, esResult, ctx2)


 -- Rule (App Synth Match)
 --  The sort of argument we have matches the mode of the function.
 | Just (paramSort, paramMode, bParam, tResult)
        <- splitParamOfType tFn
 , case (paramSort, paramMode, arg) of
        (ParamSortTerm, ParamModeExplicit, RTerm{})     -> True
        (ParamSortTerm, ParamModeImplicit, RImplicit{}) -> True
        _                                               -> False
 = do
        ctrace  $ vcat
                [ text "*>  App Synth Match"
                , text "    demand    = " <> ppr demand
                , text "    scope     = " <> ppr isScope
                , text "    tFn       = " <> ppr tFn
                , text "    arg       = " <> ppr arg
                , text "    paramSort = " <> text (show paramSort)
                , text "    paramMode = " <> text (show paramMode)
                , text "    bParam    = " <> ppr bParam
                , text "    tResult   = " <> ppr tResult
                , empty ]

        -- Check the argument.
        let tParam = typeOfBind bParam
        (arg', tArg, esArg, ctx1)
         <- checkArg table ctx0 (Check tParam) DemandRun arg

        tFn'     <- applyContext ctx1 tFn
        tArg'    <- applyContext ctx1 tArg
        tResult' <- applyContext ctx1 tResult

        -- Get the type, effect and closure resulting from the application
        -- of a function of this type to its argument.
        esLatent
         <- case splitFunType tFn' of
             Just (_tParam, effsLatent, _closLatent, _tResult)
              -> return effsLatent

             -- This shouldn't happen because this rule (App Synth Fun) only
             -- applies when 'tFn' is has a functional type, and applying
             -- the current context to it as above should not change this.
             Nothing
              -> error "ddc-core.synthAppArg: unexpected type of function."

        -- Result of evaluating the functional expression applied
        -- to its argument.
        let esExp       = Sum.unions kEffect
                        $ [ effsFn, esArg, Sum.singleton kEffect esLatent]

        -- The checked application.
        let xExp'       = XApp  (AnTEC tResult' (TSum esExp) (tBot kClosure) a)
                                xFn arg'

        -- If the function returns a suspension then automatically run it.
        let (xExpRun, tExpRun, esExpRun)
                | configImplicitRun (tableConfig table)
                , DemandRun     <- demand
                , Just (eExpRun', tExpRun') <- takeTSusp tResult'
                = let
                        eTotal  = tSum kEffect [TSum esExp, eExpRun']

                  in    ( XCast (AnTEC tResult' eTotal (tBot kClosure) a)
                                CastRun xExp'
                        , tExpRun'
                        , Sum.fromList kEffect [eTotal])

                | otherwise
                =       ( xExp'
                        , tResult'
                        , esExp)

        ctrace  $ vcat
                [ text "*<  App Synth Match"
                , indent 4 $ ppr xx
                , text "    demand  = " <> ppr demand
                , text "    scope   = " <> ppr isScope
                , text "    arg     = " <> ppr arg
                , text "    tFn'    = " <> ppr tFn'
                , text "    tArg'   = " <> ppr tArg'
                , text "    arg'    = " <> ppr arg'
                , text "    xExpRun = " <> ppr xExpRun
                , text "    tExpRun = " <> ppr tExpRun
                , indent 4 $ ppr ctx1
                , empty ]

        return  (xExpRun, tExpRun, esExpRun, ctx1)


 -- Rule (App Synth Implicit Term)
 --  Function is an implicit term abstraction, but the argument does not satisfy it.
 --  Find a term in the context of the type of the abstraction, and apply it to that.
 | Just (ParamSortTerm, ParamModeImplicit, bParam, tBody)
        <- splitParamOfType tFn
 , case arg of
        RType{}         -> True
        RTerm{}         -> True
        RWitness{}      -> True
        RImplicit{}     -> False
 = do
        ctrace  $ vcat
                [ text "*>  App Synth Implicit Term"
                , text "    demand  = " <> ppr demand
                , text "    scope   = " <> ppr isScope
                , text "    xFn     = " <> ppr xFn
                , text "    arg     = " <> ppr arg
                , empty ]

        -- Build an argument (elaborate# [t]) where t is the desired argument type.
        --  This argument will then be replaced by a real term after type checking,
        --  by the Resolve transform. Note that we can't nessesarally construct
        --  the desired term during type checking as they types we have may still
        --  contain existentials, so we might night have enough informaiton to decide
        --  whether a given binding has the desired type.
        let aArg         = AnTEC (typeOfBind  bParam)     (tBot kEffect) (tBot kClosure) a
        let aFnElab      = AnTEC (shapeOfPrim PElaborate) (tBot kEffect) (tBot kClosure) a
        let xArgImplicit = XApp aArg (XPrim aFnElab PElaborate) (RType (typeOfBind bParam))

        -- Add the implicit type argument.
        let aFn          = AnTEC tFn (TSum effsFn) (tBot kClosure) a
        let xFnArg       = XApp aFn xFn (RImplicit (RTerm xArgImplicit))

        -- Synthesise the result type of a function being applied to its
        -- argument. We know the type of the function up-front, but we pass
        -- in the whole argument expression.
        (xResult, tResult, esResult, ctx1)
         <- synthAppArg table a xx ctx0 demand isScope xFnArg tBody effsFn arg

        -- Result expression.
        ctrace  $ vcat
                [ text "*<  App Synth Implicit Term"
                , text "    demand  = " <> ppr demand
                , text "    scope   = " <> ppr isScope
                , text "    xFn     = " <> ppr xFn
                , text "    arg     = " <> ppr arg
                , text "    xResult = " <> ppr xResult
                , text "    tResult = " <> ppr tResult
                , indent 4 $ ppr ctx1
                , empty ]

        return  (xResult, tResult, esResult, ctx1)


 -- Rule (App Synth Elaborate Type)
 --  Function is an elaborative type abstraction, but the argument does not satisfy it.
 --  We need to inject a new type argument first.
 | Just (ParamSortType, ParamModeElaborate, bParam, tBody)
        <- splitParamOfType tFn
 , case arg of
        RType{}         -> False
        RTerm{}         -> True
        RWitness{}      -> True
        RImplicit{}     -> True

 = do
        -- Make a new existential for the type of the argument, and push it
        -- onto the context. The new existential may appear in some other
        -- type constraint, so make sure to add it to the correct scope.
        iA         <- newExists (typeOfBind bParam)
        let tA     =  typeOfExists    iA
        let ctx1   =  pushExistsScope iA isScope ctx0

        -- Instantiate the type of the function with the new existential.
        let tBody' =  substituteT bParam tA tBody

        -- Add the missing type application.
        --  Because we were applying a function to an expression argument,
        --  and the type of the function was quantified, we know there should
        --  be a type application here.
        let aFn    = AnTEC tFn (TSum effsFn) (tBot kClosure) a
        let xFnTy  = XApp aFn xFn (RType tA)

        ctrace  $ vcat
                [ text "*>  App Synth Elaborate Type"
                , text "    demand  = " <> ppr demand
                , text "    scope   = " <> ppr isScope
                , text "    xFn     = " <> ppr xFn
                , text "    arg     = " <> ppr arg
                , text "    iA      = " <> ppr iA
                , text "    tBody'  = " <> ppr tBody'
                , text "    xResult = " <> ppr xFnTy
                , empty ]

        -- Synthesise the result type of a function being applied to its
        -- argument. We know the type of the function up-front, but we pass
        -- in the whole argument expression.
        (xResult, tResult, esResult, ctx2)
         <- synthAppArg table a xx ctx1 demand isScope xFnTy tBody' effsFn arg

        -- Result expression.
        ctrace  $ vcat
                [ text "*<  App Synth Elaborate Type"
                , text "    demand  = " <> ppr demand
                , text "    scope   = " <> ppr isScope
                , text "    xFn     = " <> ppr xFn
                , text "    tFn     = " <> ppr tFn
                , text "    arg     = " <> ppr arg
                , text "    xResult = " <> ppr xResult
                , text "    tResult = " <> ppr tResult
                , indent 4 $ ppr ctx2
                , empty ]

        return  (xResult, tResult, esResult, ctx2)


 -- Applied expression is not a function.
 | otherwise
 =      throw $ ErrorAppNotFun a xx tFn


-------------------------------------------------------------------------------
-- | Split a function-ish type into its parts.
--   This works for implications, as well as the function constructor
--   with and without a latent effect.
splitFunType :: Type n -> Maybe (Type n, Effect n, Closure n, Type n)
splitFunType tt
 = case tt of
        TApp (TApp (TCon (TyConWitness TwConImpl)) t11) t12
          -> Just (t11, tBot kEffect, tBot kClosure, t12)

        TApp (TApp (TCon (TyConSpec TcConFunExplicit)) t11) t12
          -> Just (t11, tBot kEffect, tBot kClosure, t12)

        TApp (TApp (TCon (TyConSpec TcConFunImplicit)) t11) t12
          -> Just (t11, tBot kEffect, tBot kClosure, t12)

        _ -> Nothing


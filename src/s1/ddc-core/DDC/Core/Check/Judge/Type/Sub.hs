{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Judge.Type.Sub
        (checkSub)
where
import DDC.Core.Check.Judge.Type.Base
import DDC.Core.Check.Judge.Type.Prim
import qualified DDC.Core.Env.EnvT      as EnvT
import qualified DDC.Type.Sum           as Sum
import qualified Data.Map               as Map


-- This is the subtyping rule for the type checking judgment.
checkSub table !a ctx0 demand xx0 tExpect
 = do
        ctrace  $ vcat
                [ text "*>  Sub Check"
                , text "    demand:  " <> (text $ show demand)
                , text "    tExpect: " <> (ppr tExpect)
                , indent 4 $ ppr ctx0
                , empty ]

        let config      = tableConfig table

        -- Synthesise a type for the expression.
        (xx1, tSynth, effs1, ctx1)
         <- tableCheckExp table table
                ctx0 (Synth $ slurpExists tExpect)
                demand xx0

        -- Substitute context into synthesised and expected types.
        tSynth_ctx1     <- applyContext ctx1 tSynth
        tExpect_ctx1    <- applyContext ctx1 tExpect

        -- If the synthesised type is not quantified,
        -- but the expected one is then instantiate it at some new existentials.
        -- The expected type needs to be an existential so we know where to
        -- insert the new existentials we create into the context.
        (xx_dequant, tDequant, ctx2)
         <- case takeExists tExpect of
                Just iExpect
                 -> expliciate table a ctx1 iExpect xx1 tSynth_ctx1 tExpect_ctx1

                Nothing
                 -> return (xx1, tSynth_ctx1, ctx1)

        ctrace  $ vcat
                [ text "*.  Sub Check"
                , text "    demand:   " <> (text $ show demand)
                , text "    tExpect:  " <> ppr tExpect_ctx1
                , text "    tSynth:   " <> ppr tSynth_ctx1
                , text "    tDequant: " <> ppr tDequant
                , empty ]

        -- Make the synthesised type a subtype of the expected one.
        (xx2, effs3, ctx3)
         <- makeSub config a ctx2 xx0 xx_dequant tDequant tExpect_ctx1
         $  ErrorMismatch  a tDequant tExpect_ctx1 xx0

        let effs' = Sum.union effs1 effs3

        ctrace  $ vcat
                [ text "*<  Sub"
                , indent 4 $ ppr xx0
                , text "    tExpect:  " <> ppr tExpect
                , text "    tSynth:   " <> ppr tSynth
                , text "    tDequant: " <> ppr tDequant
                , text "    tExpect': " <> ppr tExpect_ctx1
                , text "    tSynth':  " <> ppr tSynth_ctx1
                , indent 4 $ ppr ctx0
                , indent 4 $ ppr ctx1
                , indent 4 $ ppr ctx3
                , empty ]

        returnX a
                (\_ -> xx2)
                tExpect
                effs' ctx3


-- | Insert default argument placeholders to match the implicit
--   parameters listed at the front of the expected type.
expliciate !_table !aApp ctx0 iBefore xx0 tSynth tExpect
 | TCon (TyConExists _n _k)  <- tExpect
 , shouldExpliciateX xx0
 = do
        -- Split off descriptions of implicit parameters from the front of the type.
        (bsParam, tBody)
         <- stripImplicitParams ctx0 tSynth

        -- Add default arguments for each implicit parameter.
        case bsParam of
         []     -> return (xx0, tSynth, ctx0)
         _      -> addImplicitApps aApp ctx0 iBefore xx0 (reverse bsParam) tBody

 | otherwise
 = return (xx0, tSynth, ctx0)


shouldExpliciateX :: Exp a n -> Bool
shouldExpliciateX xx
 = case xx of
        XVar{}  -> True
        XCon{}  -> True
        _       -> False


-- | Apply the given expression to existentials to instantiate its type.
--
--   The new existentials are inserted into the context just before
--   the given one so that the context scoping works out.
--
addImplicitApps
        :: Ord n
        => a                    -- ^ Annotation for new AST nodes.
        -> Context n            -- ^ Current type checker context.
        -> Exists n             -- ^ Add new existentials before this one.
        -> Exp (AnTEC a n) n    -- ^ Expression to add type applications to.
        -> [Param n]            -- ^ Implicit parameters to add arguments for.
        -> Type n               -- ^ Body of the forall.
        -> CheckM a n
                ( Exp (AnTEC a n) n
                , Type n
                , Context n)

addImplicitApps !_aApp ctx0 _ xx0 [] tBody
 = return (xx0, tBody, ctx0)

addImplicitApps !aApp  ctx0 iBefore xx0 (MType bParam : bsParam) tBody
 = do
        let kParam = typeOfBind bParam

        (xx1, tBody', ctx1)
         <- addImplicitApps aApp ctx0 iBefore xx0 bsParam tBody

        iArg        <- newExists kParam
        let tArg    = typeOfExists iArg
        let ctx2    = pushExistsBefore iArg iBefore ctx1

        let tResult = substituteT bParam tArg tBody'

        let aApp'   = AnTEC tResult (tBot kEffect) (tBot kClosure) aApp
        let xx2     = XApp aApp' xx1 (RType tArg)

        return (xx2, tResult, ctx2)

addImplicitApps aApp ctx0 iBefore xx0 (MImplicit bParam : bsParam) tBody
 = do
        (xx1, tBody', ctx1)
         <- addImplicitApps aApp ctx0 iBefore xx0 bsParam tBody

        let aArg         = AnTEC (typeOfBind  bParam)     (tBot kEffect) (tBot kClosure) aApp
        let aFnElab      = AnTEC (shapeOfPrim PElaborate) (tBot kEffect) (tBot kClosure) aApp
        let xArgImplicit = XApp aArg (XPrim aFnElab PElaborate) (RType (typeOfBind bParam))

        -- Add the implicit type argument.
        let aFn          = AnTEC tBody' (tBot kEffect) (tBot kClosure) aApp
        let xFnArg       = XApp  aFn xx1 (RImplicit (RTerm xArgImplicit))

        return (xFnArg, tBody, ctx1)


addImplicitApps _ ctx0 _ xx0 (_ : _) tBody
 = return (xx0, tBody, ctx0)


-- | Strip quantifiers from the front of a type, looking through any type synonyms.
--
--   ISSUE #385: Make type inference work for non trivial type synonyms.
--   If the synonym is higher kinded then we need to reduce the application.
--   trying to strip the TForall.
--
stripImplicitParams
        :: Ord n
        => Context n
        -> Type n
        -> CheckM a n ([Param n], Type n)

stripImplicitParams ctx tt
 = case tt of
        -- Look through type synonyms.
        TCon (TyConBound (UName n) _)
         | Just tt' <- Map.lookup n
                    $  EnvT.envtEquations $ contextEnvT ctx
         -> stripImplicitParams ctx tt'

        -- Strip quantifier.
        TForall bParam tBody
         -> do  (bsParam, tBody')
                 <- stripImplicitParams ctx tBody
                return  ( MType bParam : bsParam
                        , tBody')

        -- Strip implicit parameter.
        TApp{}
         | Just (TcConFunImplicit, tParam, tResult) <- takeTFunCon tt
         -> do  (bsParam, tResult')
                 <- stripImplicitParams ctx tResult
                return  ( MImplicit (BNone tParam) : bsParam
                        , tResult')

        _ ->    return ([], tt)



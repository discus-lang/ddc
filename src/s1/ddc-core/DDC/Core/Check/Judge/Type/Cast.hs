{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Judge.Type.Cast
        ( checkCast)
where
import DDC.Core.Check.Judge.Kind
import DDC.Core.Check.Judge.EqT
import DDC.Core.Check.Judge.Type.Sub
import DDC.Core.Check.Judge.Type.Base
import qualified DDC.Type.Sum   as Sum


checkCast :: Checker a n

-- WeakenEffect ---------------------------------------------------------------
-- Weaken the effect of an expression.
checkCast !table !ctx0
        mode _demand
        xx@(XCast a (CastWeakenEffect eff) x1)
 = do   let config      = tableConfig  table

        -- Check the effect term.
        (eff', kEff, ctx1)
         <- checkTypeM config ctx0 UniverseSpec eff
          $ case mode of
                Recon   -> Recon
                Synth{} -> Check kEffect
                Check{} -> Check kEffect

        -- Check the body.
        (x1', t1, effs, ctx2)
         <- tableCheckExp table table ctx1 mode DemandNone x1

        -- The effect term must have Effect kind.
        when (not $ isEffectKind kEff)
         $ throw $ ErrorMismatch a kEff kEffect xx

        let c'     = CastWeakenEffect eff'
        let effs'  = Sum.insert eff' effs

        returnX a (\z -> XCast z c' x1')
                t1 effs' ctx2


-- Purify ---------------------------------------------------------------------
-- Purify the effect of an expression.
--
-- EXPERIMENTAL: The Tetra language doesn't have purification casts yet,
--               so proper type inference isn't implemented.
--
checkCast !table !ctx
        mode _demand
        xx@(XCast a (CastPurify w) x1)
 = do   let config      = tableConfig table

        -- Check the witness.
        (w', tW)  <- checkWitnessM config ctx w
        let wTEC  = reannotate fromAnT w'

        -- Check the body.
        (x1', t1, effs, ctx1)
         <- tableCheckExp table table ctx mode DemandNone x1

        -- The witness must have type (Pure e), for some effect e.
        effs' <- case tW of
                  TApp (TCon (TyConWitness TwConPure)) effMask
                    -> return $ Sum.delete effMask effs
                  _ -> throw  $ ErrorWitnessNotPurity a xx w tW

        let c'  = CastPurify wTEC
        returnX a (\z -> XCast z c' x1')
                t1 effs' ctx1


-- Box ------------------------------------------------------------------------
-- Box a computation,
-- capturing its effects in a computation type.
checkCast !table ctx0
        mode _demand
        xx@(XCast a CastBox x1)
 = case mode of
    Check tExpected
     -> do
        let config      = tableConfig table

        -- Check the body.
        (x1', tBody, effs, ctx1)
         <- tableCheckExp table table ctx0
                (Synth $ slurpExists tExpected) DemandRun x1

        let effs_crush
                = Sum.fromList kEffect
                [ crushEffect (contextEnvT ctx0) (TSum effs)]

        -- The actual type is (S eff tBody).
        tBody'      <- applyContext ctx1 tBody
        let tActual =  tApps (TCon (TyConSpec TcConSusp))
                             [TSum effs_crush, tBody']

        -- The actual type needs to match the expected type.
        -- We're treating the S constructor as invariant in both positions,
        --  so we use 'makeEq' here instead of 'makeSub'
        tExpected'  <- applyContext ctx1 tExpected
        ctx2        <- makeEqT config ctx1 tActual tExpected'
                    $  ErrorMismatch  a    tActual tExpected' xx

        returnX a (\z -> XCast z CastBox x1')
                tExpected (Sum.empty kEffect) ctx2

    -- Recon and Synth mode.
    _
     -> do
        -- Check the body.
        (x1', t1, effs,  ctx1)
         <- tableCheckExp table table ctx0 mode DemandRun x1

        let effs_crush
                = Sum.fromList kEffect
                [ crushEffect (contextEnvT ctx1) (TSum effs)]

        -- The result type is (S effs a).
        let tS  = tApps (TCon (TyConSpec TcConSusp))
                        [TSum effs_crush, t1]

        returnX a (\z -> XCast z CastBox x1')
                tS (Sum.empty kEffect) ctx1


-- Run ------------------------------------------------------------------------
-- Run a suspended computation,
-- releasing its effects into the environment.
checkCast !table !ctx0 mode _demand
        xx@(XCast a CastRun xBody)
 = case mode of
    Recon
     -> do
        -- Check the body.
        (xBody', tBody, effs, ctx1)
         <- tableCheckExp table table ctx0 Recon DemandNone xBody

        -- The body must have type (S eff a),
        --  and the result has type 'a' while unleashing effect 'eff'.
        case tBody of
         TApp (TApp (TCon (TyConSpec TcConSusp)) eff2) tResult
          -> do
                -- Check that the context has the capability to support
                -- this effect.
                let config      = tableConfig table
                checkEffectSupported config a xx ctx0 eff2

                returnX a
                        (\z -> XCast z CastRun xBody')
                        tResult
                        (Sum.union effs (Sum.singleton kEffect eff2))
                        ctx1

         _ -> throw $ ErrorRunNotSuspension a xx tBody

    Synth{}
     -> do
        -- Synthesize a type for the body.
        (xBody', tBody, effs, ctx1)
         <- tableCheckExp table table ctx0
                mode DemandNone xBody

        -- Run the body,
        -- which needs to have been resolved to a computation type.
        tBody'  <- applyContext ctx1 tBody
        (tResult, effsSusp, ctx2)
         <- synthRunSusp table a xx ctx1 tBody'

        returnX a
                (\z -> XCast z CastRun xBody')
                tResult
                (Sum.union effs (Sum.singleton kEffect effsSusp))
                ctx2

    Check tExpected
     -> checkSub table a ctx0 DemandNone xx tExpected

checkCast _ _ _ _ _
        = error "ddc-core.checkCast: no match"


-------------------------------------------------------------------------------
-- | Synthesize the type of a run computation.
synthRunSusp
        :: (Show n, Ord n, Pretty n)
        => Table a n
        -> a                    -- Annot for error messages.
        -> Exp a n              -- Cast expression for error messages.
        -> Context n            -- Current context.
        -> Type n               -- Type of suspended computation.
        -> CheckM a n
                ( Type n        -- Type of result value.
                , Effect n      -- Effects unleashed by running the computation.
                , Context n)    -- Result context.

synthRunSusp table a xx ctx0 tt

 -- Rule (Run Synth exists)
 -- If the type of the suspension has not been resolved then we don't know
 -- what effects it has, and thus cannot check if running them is supported
 -- by the context.
 | Just _iFn     <- takeExists tt
 =      throw $ ErrorRunCannotInfer a xx

 -- Rule (Run Synth Susp)
 | TApp (TApp (TCon (TyConSpec TcConSusp)) eff) tResult <- tt
 = do
        -- Check that the context has the capability to support this effect.
        let config      = tableConfig table
        checkEffectSupported config a xx ctx0 eff

        return (tResult, eff, ctx0)

 -- Run expression is not a suspension.
 | otherwise
 =      throw $ ErrorRunNotSuspension a xx tt


-- Support --------------------------------------------------------------------
-- | Check if the provided effect is supported by the context,
--   if not then throw an error.
checkEffectSupported
        :: (Show n, Ord n)
        => Config n             -- ^ Static config.
        -> a                    -- ^ Annotation for error messages.
        -> Exp a n              -- ^ Expression for error messages.
        -> Context n            -- ^ Input context.
        -> Effect n             -- ^ Effect to check
        -> CheckM a n ()

checkEffectSupported _config a xx ctx eff
 = case effectSupported ctx eff of
        Nothing         -> return ()
        Just effBad     -> throw $ ErrorRunNotSupported a xx effBad



{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Judge.Type.VarCon
        (checkVarCon)
where
import DDC.Core.Check.Judge.Type.DaCon
import DDC.Core.Check.Judge.Type.Sub
import DDC.Core.Check.Judge.Type.Base
import qualified DDC.Core.Env.EnvX      as EnvX
import qualified DDC.Type.Sum           as Sum


-------------------------------------------------------------------------------
checkVarCon :: Checker a n

-- variables ------------------------------------
checkVarCon !table !ctx mode demand xx@(XVar a u)

 -- Look in the local context.
 | Just t       <- lookupType u ctx
 = case mode of
        -- Check subsumption against an existing type.
        -- This may instantiate existentials in the exising type.
        Check tExpect
         ->     checkSub table a ctx demand xx tExpect

        _ -> do ctrace  $ vcat
                        [ text "**  Var Local"
                        , indent 4 $ ppr xx
                        , text "    tVar: " <> ppr t
                        , indent 4 $ ppr ctx
                        , mempty ]

                returnX a
                        (\z -> XVar z u)
                        t
                        (Sum.empty kEffect)
                        ctx

 -- Look in the global environment.
 | Just t      <- EnvX.lookupX u (contextEnvX ctx)
 = case mode of
        -- Check subsumption against an existing type.
        -- This may instantiate existentials in the exising type.
        Check tExpect
         ->     checkSub table a ctx demand xx tExpect

        _ -> do ctrace  $ vcat
                        [ text "**  Var Global"
                        , indent 4 $ ppr xx
                        , text "    tVar: " <> ppr t
                        , indent 4 $ ppr ctx
                        , mempty ]

                returnX a
                        (\z -> XVar z u)
                        t
                        (Sum.empty kEffect)
                        ctx

 -- Can't find this variable name in the environment.
 | otherwise
 = throw $ ErrorUndefinedVar a u UniverseData


-- constructors ---------------------------------
-- Recon or Synth the type of a constructor.
checkVarCon !table !ctx mode@Recon _demand xx@(XCon a dc)
 = do   let config      = tableConfig table

        -- Lookup the type of the constructor from the environment.
        (dc', tCtor) <- checkDaConM config ctx xx a dc

        ctrace  $ vcat
                [ text "**  Con"
                , text "    MODE: " <> ppr mode
                , indent 4 $ ppr xx
                , text "    tCon: " <> ppr tCtor
                , indent 4 $ ppr ctx
                , mempty ]

        -- Type of the data constructor.
        returnX a
                (\z -> XCon z dc')
                tCtor
                (Sum.empty kEffect)
                ctx


-- Check a constructor against an expected type.
checkVarCon !table !ctx (Check tExpect) demand xx@(XCon a _)
 = checkSub table a ctx demand xx tExpect


-- Synthesise the type of a data constructor.
checkVarCon !table !ctx mode@(Synth {}) _demand xx@(XCon a dc)
 = do
        let config      = tableConfig table

        -- All data constructors need to have valid type annotations.
        (dc', tCtor) <- checkDaConM config ctx xx a dc

        ctrace  $ vcat
                [ text "**  Con"
                , text "    MODE: " <> ppr mode
                , indent 4 $ ppr xx
                , text "    tCon: " <> ppr tCtor
                , indent 4 $ ppr ctx
                , mempty ]

        -- Type of the data constructor.
        returnX a
                (\z -> XCon z dc')
                tCtor
                (Sum.empty kEffect)
                ctx


-- others ---------------------------------------
checkVarCon _ _ _ _ _
 = error "ddc-core.checkVarCon: no match"


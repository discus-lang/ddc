
module DDC.Core.Check.Judge.Type.VarCon
        (checkVarCon)
where
import DDC.Core.Check.Judge.Type.DaCon
import DDC.Core.Check.Judge.Type.Sub
import DDC.Core.Check.Judge.Type.Base
import qualified DDC.Core.Env.EnvX      as EnvX
import qualified DDC.Type.Sum           as Sum
import qualified Data.Map               as Map


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
                        , empty ]

                returnX a
                        (\z -> XVar z u)
                        t
                        (Sum.empty kEffect)
                        ctx

 -- Look in the global environment.
 | Just t      <- EnvX.lookup u (contextEnvX ctx)
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
                        , empty ]

                returnX a
                        (\z -> XVar z u)
                        t
                        (Sum.empty kEffect)
                        ctx

 -- Can't find this variable name in the environment.
 | otherwise
 = throw $ ErrorUndefinedVar a u UniverseData


-- constructors ---------------------------------
checkVarCon !table !ctx mode demand xx@(XCon a dc) 
 -- For recon and synthesis we already know what type the constructor
 -- should have, so we can use that.
 | mode == Recon || mode == Synth
 = do
        let config      = tableConfig table
        let defs        = configDataDefs config

        -- All data constructors need to have valid type annotations.
        tCtor
         <- case dc of
             DaConUnit   -> return tUnit

             DaConPrim{} -> return $ daConType dc

             DaConBound n
              -- Types of algebraic data ctors should be in the defs table.
              |  Just ctor <- Map.lookup n (dataDefsCtors defs)
              -> return $ typeOfDataCtor ctor

              | otherwise
              -> throw  $ ErrorUndefinedCtor a $ XCon a dc

        -- Check that the constructor is in the data type declarations.
        checkDaConM config xx a dc

        ctrace  $ vcat
                [ text "**  Con"
                , indent 4 $ ppr xx
                , text "    tCon: " <> ppr tCtor
                , indent 4 $ ppr ctx
                , empty ]

        -- Type of the data constructor.
        returnX a
                (\z -> XCon z dc)
                tCtor
                (Sum.empty kEffect)
                ctx

 -- Check subsumption against an existing type.
 -- This may instantiate existentials in the exising type.
 | otherwise
 , Check tExpect    <- mode
 = checkSub table a ctx demand xx tExpect


-- others ---------------------------------------
checkVarCon _ _ _ _ _
 = error "ddc-core.checkVarCon: no match"


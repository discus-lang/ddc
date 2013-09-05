
module DDC.Core.Check.Exp.VarCon
        (checkVarCon)
where
import DDC.Core.Check.DaCon
import DDC.Core.Check.Exp.Base
import qualified DDC.Type.Env   as Env
import qualified DDC.Type.Sum   as Sum
import qualified Data.Map       as Map
import qualified Data.Set       as Set


checkVarCon :: Checker a n

-- variables ------------------------------------
checkVarCon !_table !_kenv !tenv !ctx (XVar a u) _
 = case Env.lookup u tenv of
        Nothing -> throw $ ErrorUndefinedVar a u UniverseData
        Just t  
         -> returnX a 
                (\z -> XVar z u)
                t
                (Sum.empty kEffect)
                (Set.singleton $ taggedClosureOfValBound t u)
                ctx


-- constructors ---------------------------------
checkVarCon !table !_kenv !_tenv !ctx xx@(XCon a dc) _
 = do   let config      = tableConfig table
        let defs        = configDataDefs config

        -- TODO: this is duplicated in the case for XCase.
        --       split into another function.
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

        -- Type of the data constructor.
        returnX a
                (\z -> XCon z dc)
                tCtor
                (Sum.empty kEffect)
                Set.empty
                ctx

-- others ---------------------------------------
checkVarCon _ _ _ _ _ _
 = error "ddc-core.checkVarCon: no match"

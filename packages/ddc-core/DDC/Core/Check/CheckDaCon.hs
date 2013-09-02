
module DDC.Core.Check.CheckDaCon
        (checkDaConM)
where
import DDC.Core.Check.Error
import DDC.Core.Check.CheckWitness
import DDC.Core.Exp.DaCon
import DDC.Core.Exp
import DDC.Type.Compounds
import DDC.Type.DataDef
import DDC.Control.Monad.Check  (throw)
import Control.Monad
import Prelude                  as L
import qualified Data.Map       as Map


-- | Check a data constructor, returning its type.
checkDaConM
        :: (Ord n, Eq n, Show n)
        => Config n
        -> Exp a n              -- ^ The full expression for error messages.
        -> DaCon n              -- ^ Data constructor to check.
        -> CheckM a n (Type n)

checkDaConM config xx dc
 = case dc of
    -- Type type of the unit data constructor is baked-in.
    DaConUnit
     -> return tUnit

    -- Primitive data constructors which are not algebraic do not need a
    --   data type declaration. These are things like Float literals, or fragment
    --   specific magic values that we can't destruct with case-expressions.
    DaConPrim { daConType = t
              , daConIsAlgebraic = False } 
     -> return t

    -- Primitive data constructors which are algebraic need to have a
    --   corresponding data type definition.
    --    If they are 'Small' the data constructor needs to be listed,
    --    If they are 'Large' (like Ints) there are too many to enumerate.
    DaConPrim { daConName        = nCtor
              , daConType        = t
              , daConIsAlgebraic = True  }
     -> let  tResult = snd $ takeTFunArgResult $ eraseTForalls t
             defs    = configDataDefs config

        in case liftM fst $ takeTyConApps tResult of
            Just (TyConBound u _)
                | Just nType         <- takeNameOfBound u
                , Just dataType      <- Map.lookup nType (dataDefsTypes defs)
                -> case dataTypeMode dataType of
                    DataModeSmall nsCtors
                        | L.elem nCtor nsCtors -> return t
                        | otherwise            -> throw $ ErrorUndefinedCtor xx

                    DataModeLarge   -> return t

            _ -> throw $ ErrorUndefinedCtor xx

    -- Bound data constructors are always algebraic and Small, so there needs
    --   to be a data definition that gives the type of the constructor.
    DaConBound { daConName = nCtor }
     -> case Map.lookup nCtor (dataDefsCtors $ configDataDefs config) of
         Just ctor       -> return $ typeOfDataCtor ctor
         Nothing         -> throw $ ErrorUndefinedCtor xx


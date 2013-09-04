
module DDC.Core.Check.DaCon
        (checkDaConM)
where
import DDC.Core.Check.Error
import DDC.Core.Check.Witness
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

    -- Primitive data constructors need to have a corresponding data type,
    -- but there may be too many constructors to list, like with Int literals. 
    --
    -- The mode field in the data type declaration says what to expect.
    --    If the mode is 'Small' the data constructor needs to be listed,
    --    If the mode is 'Large' (like Int) there are too many to enumerate.
    --
    -- The type of the constructor needs to be attached so we can determine
    --  what data type it belongs to.
    DaConPrim { daConName        = nCtor
              , daConType        = t }
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


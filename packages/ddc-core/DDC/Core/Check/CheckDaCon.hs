
module DDC.Core.Check.CheckDaCon
        (checkDaConM)
where
import DDC.Core.Check.Error
import DDC.Core.Check.CheckWitness
import DDC.Core.DaCon
import DDC.Core.Exp
import DDC.Type.Check.Monad     (throw)
import DDC.Type.Compounds
import DDC.Type.DataDef
import Control.Monad
import Prelude                  as L
import qualified Data.Map       as Map


-- | Check a data constructor.
--   The data constructor must be in the set of data type declarations.
checkDaConM
        :: (Ord n, Eq n, Show n)
        => Config n
        -> Exp a n              -- ^ The full expression for error messages.
        -> DaCon n              -- ^ Data constructor to check.
        -> CheckM a n ()

checkDaConM _ _ dc
 | DaConUnit    <- daConName dc
 = return ()

checkDaConM config xx dc
 | DaConNamed nCtor <- daConName dc
 , daConIsAlgebraic dc
 = let  tResult = snd $ takeTFunArgResult $ eraseTForalls $ typeOfDaCon dc
        defs    = configPrimDataDefs config
   in   case liftM fst $ takeTyConApps tResult of
         Just (TyConBound u _)
           | Just nType         <- takeNameOfBound u
           , Just dataType      <- Map.lookup nType (dataDefsTypes defs)
           -> case dataTypeMode dataType of
                DataModeSmall nsCtors
                 | L.elem nCtor nsCtors  -> return ()
                 | otherwise    -> throw $ ErrorUndefinedCtor xx

                DataModeLarge   -> return ()

         _ -> throw $ ErrorUndefinedCtor xx

checkDaConM _ _ _
 = return ()


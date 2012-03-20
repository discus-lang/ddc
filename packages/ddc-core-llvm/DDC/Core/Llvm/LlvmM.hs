
module DDC.Core.Llvm.LlvmM
        ( LlvmM
        , LlvmState(..)
        , llvmStateInit 
        , die

          -- * Uniques
        , newUnique
        , newUniqueVar
        , newUniqueNamedVar
        , newUniqueLabel)
where
import DDC.Llvm.Type
import DDC.Llvm.Var
import Control.Monad.State.Strict

type LlvmM = State LlvmState


-- LlvmState ------------------------------------------------------------------
-- | State for the LLVM conversion.
data LlvmState
        = LlvmState
        { -- Unique name generator.
          llvmStateUnique       :: Int }


-- | Initial LLVM state.
llvmStateInit :: LlvmState
llvmStateInit
        = LlvmState
        { llvmStateUnique       = 1 }


-- | Called when we find a thing that cannot be converted to C.
die :: String -> a
die msg = error $ "DDC.Core.Llvm.Convert " ++ msg


-- Unique ---------------------------------------------------------------------
-- | Unique name generation.
newUnique :: String -> LlvmM Unique
newUnique name 
 = do   s       <- get
        let u   = llvmStateUnique s
        put     $ s { llvmStateUnique = u + 1 }
        return  $ Unique u name        


-- | Generate a new unique register variable with the specified `LlvmType`.
newUniqueVar :: LlvmType -> LlvmM Var
newUniqueVar t
 = do   u <- newUnique "r"
        return $ VarLocal u t


-- | Generate a new unique named register variable with the specified `LlvmType`.
newUniqueNamedVar :: String -> LlvmType -> LlvmM Var
newUniqueNamedVar name t
 = do   u <- newUnique name
        return $ VarLocal u t


-- | Generate a new unique label.
newUniqueLabel :: String -> LlvmM Var
newUniqueLabel name
 = do   u <- newUnique name
        return $ VarLocal u LMLabel

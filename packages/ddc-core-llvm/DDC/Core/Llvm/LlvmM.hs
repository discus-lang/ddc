
module DDC.Core.Llvm.LlvmM
        ( LlvmM
        , LlvmState(..)
        , llvmStateInit 

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


-- Unique ---------------------------------------------------------------------
-- | Unique name generation.
newUnique :: String -> LlvmM Unique
newUnique name 
 = do   s       <- get
        let u   = llvmStateUnique s
        put     $ s { llvmStateUnique = u + 1 }
        return  $ Unique u name        


-- | Generate a new unique register variable with the specified `LlvmType`.
newUniqueVar :: LlvmType -> LlvmM LlvmVar
newUniqueVar t
 = do   u <- newUnique "r"
        return $ LMLocalVar u t


-- | Generate a new unique named register variable with the specified `LlvmType`.
newUniqueNamedVar :: String -> LlvmType -> LlvmM LlvmVar
newUniqueNamedVar name t
 = do   u <- newUnique name
        return $ LMLocalVar u t


-- | Generate a new unique label.
newUniqueLabel :: String -> LlvmM LlvmVar
newUniqueLabel name
 = do   u <- newUnique name
        return $ LMLocalVar u LMLabel

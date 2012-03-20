
module DDC.Core.Llvm.LlvmM
        ( LlvmM
        , LlvmState(..)
        , llvmStateInit 
        , die

          -- * Uniques
        , newUnique
        , newUniqueVar
        , newUniqueNamedVar
        , newUniqueLabel
        , newUniqueBlockId)
where
import DDC.Core.Llvm.Platform
import DDC.Llvm.Stmt
import DDC.Llvm.Type
import DDC.Llvm.Var
import Control.Monad.State.Strict

type LlvmM = State LlvmState

-- | Called when we find a thing that cannot be converted to C.
die :: String -> a
die msg = error $ "DDC.Core.Llvm.Convert " ++ msg


-- LlvmState ------------------------------------------------------------------
-- | State for the LLVM conversion.
data LlvmState
        = LlvmState
        { -- Unique name generator.
          llvmStateUnique       :: Int 

          -- The current platform.
        , llvmStatePlatform      :: Platform }


-- | Initial LLVM state.
llvmStateInit :: Platform -> LlvmState
llvmStateInit platform
        = LlvmState
        { llvmStateUnique       = 1 
        , llvmStatePlatform     = platform }


-- Unique ---------------------------------------------------------------------
-- | Unique name generation.
newUnique :: LlvmM Unique
newUnique 
 = do   s       <- get
        let u   = llvmStateUnique s
        put     $ s { llvmStateUnique = u + 1 }
        return  $ u


-- | Generate a new unique register variable with the specified `LlvmType`.
newUniqueVar :: Type -> LlvmM Var
newUniqueVar t
 = do   u <- newUnique
        return $ VarLocal ("_v" ++ show u) t


-- | Generate a new unique named register variable with the specified `LlvmType`.
newUniqueNamedVar :: String -> Type -> LlvmM Var
newUniqueNamedVar name t
 = do   u <- newUnique 
        return $ VarLocal ("_v" ++ show u ++ "_" ++ name) t


-- | Generate a new unique label.
newUniqueLabel :: String -> LlvmM Var
newUniqueLabel name
 = do   u <- newUnique
        return $ VarLocal ("_l" ++ show u ++ "_" ++ name) TLabel


-- | Generate a new unique blockid.
newUniqueBlockId :: LlvmM BlockId
 = do   u <- newUnique
        return   $ BlockId u

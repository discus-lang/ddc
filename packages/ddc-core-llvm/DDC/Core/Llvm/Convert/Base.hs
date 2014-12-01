
module DDC.Core.Llvm.Convert.Base
        ( ConvertM
        , LlvmState(..)
        , llvmStateInit 
        , throw

          -- * Uniques
        , newUnique
        , newUniqueVar
        , newUniqueNamedVar
        , newUniqueLabel)
where
import DDC.Llvm.Syntax
import DDC.Control.Monad.Check


type ConvertM = CheckM LlvmState String


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
        { llvmStateUnique       = 1  }


-- Unique ---------------------------------------------------------------------
-- | Unique name generation.
newUnique :: ConvertM Int
newUnique 
 = do   s       <- get
        let u   = llvmStateUnique s
        put     $ s { llvmStateUnique = u + 1 }
        return  $ u


-- | Generate a new unique register variable with the specified `LlvmType`.
newUniqueVar :: Type -> ConvertM Var
newUniqueVar t
 = do   u <- newUnique
        return $ Var (NameLocal ("_v" ++ show u)) t


-- | Generate a new unique named register variable with the specified `LlvmType`.
newUniqueNamedVar :: String -> Type -> ConvertM Var
newUniqueNamedVar name t
 = do   u <- newUnique 
        return $ Var (NameLocal ("_v" ++ show u ++ "." ++ name)) t


-- | Generate a new unique label.
newUniqueLabel :: String -> ConvertM Label
newUniqueLabel name
 = do   u <- newUnique
        return $ Label ("l" ++ show u ++ "." ++ name)


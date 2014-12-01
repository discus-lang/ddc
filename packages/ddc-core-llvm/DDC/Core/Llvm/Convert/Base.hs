
module DDC.Core.Llvm.Convert.Base
        ( LlvmM
        , LlvmState(..)
        , llvmStateInit 
        , die
        , dieDoc

          -- * Uniques
        , newUnique
        , newUniqueVar
        , newUniqueNamedVar
        , newUniqueLabel)
where
import DDC.Llvm.Syntax
import DDC.Base.Pretty
import DDC.Control.Monad.Check

type LlvmM = CheckM LlvmState String


-- | Called when we find a thing that cannot be converted to Llvm.
die :: String -> a
die msg = dieDoc (text msg)

dieDoc :: Doc -> a
dieDoc msg 
        = error $ renderIndent
        $    text "DDC.Core.Llvm.Convert LLVM conversion failed"
        <$$> msg


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
newUnique :: LlvmM Int
newUnique 
 = do   s       <- get
        let u   = llvmStateUnique s
        put     $ s { llvmStateUnique = u + 1 }
        return  $ u


-- | Generate a new unique register variable with the specified `LlvmType`.
newUniqueVar :: Type -> LlvmM Var
newUniqueVar t
 = do   u <- newUnique
        return $ Var (NameLocal ("_v" ++ show u)) t


-- | Generate a new unique named register variable with the specified `LlvmType`.
newUniqueNamedVar :: String -> Type -> LlvmM Var
newUniqueNamedVar name t
 = do   u <- newUnique 
        return $ Var (NameLocal ("_v" ++ show u ++ "." ++ name)) t


-- | Generate a new unique label.
newUniqueLabel :: String -> LlvmM Label
newUniqueLabel name
 = do   u <- newUnique
        return $ Label ("l" ++ show u ++ "." ++ name)


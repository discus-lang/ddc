
module DDC.Core.Llvm.Convert.Base
        ( ConvertM
        , LlvmState(..)
        , llvmStateInit 

          -- * Errors
        , Error (..)
        , throw

          -- * Uniques
        , newUnique
        , newUniqueVar
        , newUniqueNamedVar
        , newUniqueLabel

          -- * Constants
        , addConstant)
where
import DDC.Core.Llvm.Convert.Error
import DDC.Llvm.Syntax
import DDC.Control.Monad.Check
import Data.Map                 (Map)
import qualified Data.Map       as Map


-- ConvertM ---------------------------------------------------------------------------------------
-- | The toLLVM conversion monad.
type ConvertM = CheckM LlvmState Error


-- LlvmState --------------------------------------------------------------------------------------
-- | State for the LLVM conversion.
data LlvmState
        = LlvmState
        { -- Unique name generator.
          llvmStateUnique       :: Int 

          -- String and array constants collected from the code during conversion.
          -- These get stored in statically allocated memory.
        , llvmConstants         :: Map Var Lit }


-- | Initial LLVM state.
llvmStateInit :: LlvmState
llvmStateInit 
        = LlvmState
        { llvmStateUnique       = 1  
        , llvmConstants         = Map.empty }


-- Unique -----------------------------------------------------------------------------------------
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


---------------------------------------------------------------------------------------------------
-- | Add a static constant to the map, 
--   assigning a new variable to refer to it.
addConstant :: Lit -> ConvertM Var
addConstant lit
 = do   v       <- newUniqueVar (typeOfLit lit)
        s       <- get
        put     $ s { llvmConstants = Map.insert v lit (llvmConstants s)}
        return v


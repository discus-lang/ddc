
module DDC.Core.Llvm.LlvmM
        ( LlvmM
        , LlvmState(..)
        , llvmStateInit 
        , die
        , dieDoc

          -- * Uniques
        , newUnique
        , newUniqueVar
        , newUniqueNamedVar
        , newUniqueLabel

          -- * Platform Specific
        , getPrimDeclM
        , getBytesOfTypeM)
where
import DDC.Core.Salt.Platform
import DDC.Llvm.Instr
import Data.Map                 (Map)
import qualified Data.Map       as Map
import Control.Monad.State.Strict
import DDC.Base.Pretty

type LlvmM = State LlvmState


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
          llvmStateUnique       :: Int 

          -- The current platform.
        , llvmStatePlatform     :: Platform 

          -- Primitives in the global environment.
        , llvmStatePrimDecls    :: Map String FunctionDecl }


-- | Initial LLVM state.
llvmStateInit 
        :: Platform 
        -> Map String FunctionDecl 
        -> LlvmState

llvmStateInit platform prims
        = LlvmState
        { llvmStateUnique       = 1 
        , llvmStatePlatform     = platform
        , llvmStatePrimDecls    = prims }


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



-- Platform Specific ----------------------------------------------------------
-- | Get the declaration of a primitive function
getPrimDeclM :: String -> LlvmM (Maybe FunctionDecl)
getPrimDeclM name
 = do   prims   <- gets llvmStatePrimDecls
        return  $ Map.lookup name prims 


-- | Get the size of a type on this platform, in bytes.
getBytesOfTypeM :: Type -> LlvmM Integer
getBytesOfTypeM tt
 = do   platform        <- gets llvmStatePlatform
        let Just bytes  = takeBytesOfType (platformAddrBytes platform) tt
        return bytes


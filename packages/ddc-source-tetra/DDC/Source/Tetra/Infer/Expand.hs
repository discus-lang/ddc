
module DDC.Source.Tetra.Infer.Expand
        ( Config        (..)
        , configDefault
        , Expand        (..))
where
import DDC.Source.Tetra.Module
import DDC.Source.Tetra.Name
import DDC.Source.Tetra.Exp
import DDC.Type.Env                     (KindEnv, TypeEnv)
-- import qualified DDC.Type.Env           as Env


-------------------------------------------------------------------------------
data Config a n
        = Config
        { configMakeTypeHole    :: Kind n -> Type n }


configDefault :: Config a Name
configDefault 
        = Config
        { configMakeTypeHole    = \k -> TVar (UPrim NameHole k)}


-------------------------------------------------------------------------------
class Expand (c :: * -> * -> *) where
 expand
        :: Config a n
        -> KindEnv n -> TypeEnv n
        -> c a n     -> c a n
 
instance Expand Module where
 expand _config _kenv _tenv mm
        = mm



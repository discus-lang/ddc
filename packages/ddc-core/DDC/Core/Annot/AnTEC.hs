
module DDC.Core.Annot.AnTEC
        ( AnTEC (..)
        , fromAnT)
where
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Base.Pretty
import Control.DeepSeq
import Data.Typeable
import DDC.Core.Annot.AnT       (AnT)
import qualified DDC.Core.Annot.AnT as AnT


-- Annot ----------------------------------------------------------------------
-- | The type checker adds this annotation to every node in the AST, 
--   giving its type, effect and closure.
---
--   NOTE: We want to leave the components lazy so that the checker
--         doesn't actualy need to produce the type components if they're
--         not needed.
data AnTEC a n
        = AnTEC
        { annotType     :: (Type    n)
        , annotEffect   :: (Effect  n)
        , annotClosure  :: (Closure n)
        , annotTail     :: a }
        deriving (Show, Typeable)


-- | Promote an `AnT` to an `AnTEC` by filling in the effect and closure
--   portions with bottoms.
fromAnT :: AnT a n -> AnTEC a n
fromAnT (AnT.AnT t a)
   =    (AnTEC t (tBot kEffect) (tBot kClosure) a)


instance (NFData a, NFData n) => NFData (AnTEC a n) where
 rnf !an
        =     rnf (annotType    an)
        `seq` rnf (annotEffect  an)
        `seq` rnf (annotClosure an)
        `seq` rnf (annotTail    an)


instance Pretty (AnTEC a n) where
 ppr _ = text "AnTEC"        




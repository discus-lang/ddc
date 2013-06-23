
module DDC.Core.Exp.AnT
        (AnT (..))
where
import DDC.Type.Exp
import DDC.Base.Pretty
import Control.DeepSeq
import Data.Typeable


-- Annot ----------------------------------------------------------------------
-- | The type checker for witnesses adds this annotation to every node in the,
--   giving the type of each component of the witness.
---
--   NOTE: We want to leave the components lazy so that the checker
--         doesn't actualy need to produce the type components if they're
--         not needed.
data AnT a n
        = AnT
        { annotType     :: (Type  n)
        , annotTail     :: a }
        deriving (Show, Typeable)


instance (NFData a, NFData n) => NFData (AnT a n) where
 rnf !an
        =     rnf (annotType    an)
        `seq` rnf (annotTail    an)


instance Pretty (AnT a n) where
 ppr _ = text "AnT"        




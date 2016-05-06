
module DDC.Type.Exp.Flat.Pretty
        (module DDC.Type.Exp.Generic.Pretty)
where
import DDC.Type.Exp.Generic.Pretty
import DDC.Base.Pretty
import Data.Text                (Text)
import qualified Data.Text      as T


instance Pretty Text where
 ppr tt = text $ T.unpack tt


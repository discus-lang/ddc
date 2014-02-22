
module DDC.Core.Tetra.Error 
        (Error (..))
where
import DDC.Base.Pretty


-- | Fragment specific errors.
data Error a
        = Error
        deriving Show


instance Pretty (Error a) where
 ppr Error  = text (show Error)

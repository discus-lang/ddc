
module DDC.Core.Fragment.Error
        (Error(..))
where
import DDC.Core.Fragment.Feature
import DDC.Core.Exp
import DDC.Base.Pretty


data Error n
        = ErrorUnsupported      Feature
        | ErrorUndefinedPrim    n 
        | ErrorShadowedBind     n
        | ErrorUnusedBind       n
        | ErrorNakedType        (Type    n)
        | ErrorNakedWitness     (Witness n)
        deriving (Eq, Show)

instance Show n => Pretty (Error n) where
 ppr err        = text (show err)



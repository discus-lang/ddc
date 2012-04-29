
module DDC.Core.Brine.Lite.Convert
        (toBrine)
where
import DDC.Core.Module
--import DDC.Core.Exp
import qualified DDC.Core.Brine.Lite.Name         as L
import qualified DDC.Core.Brine.Output.Name       as O


toBrine :: Module a L.Name -> Module a O.Name
toBrine _mm = error "toBrine: finish me"

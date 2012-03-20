
module DDC.Core.Sea.Output.Error
        (Error(..))
where
import DDC.Core.Sea.Output.Name
import DDC.Core.Module
import DDC.Base.Pretty


data Error a
        = ErrorNoTopLevelLetrec
        { errorModule   :: Module a Name }

instance Pretty a => Pretty (Error a) where
 ppr err
  = case err of
        ErrorNoTopLevelLetrec _mm
         -> vcat [ text "Module does not have a top-level letrec."
                 , empty ]
--                 , text "with:"                         <> align (ppr mm) ]
                -- TODO: need pretty printer for modules.

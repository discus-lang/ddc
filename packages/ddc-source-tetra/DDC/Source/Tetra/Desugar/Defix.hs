
module DDC.Source.Tetra.Desugar.Defix
        ( Error         (..)
        , defixModule)
where
import DDC.Source.Tetra.Module

data Error
        = ErrorBlerk
        deriving Show

defixModule :: Module a n -> Either Error (Module a n)
defixModule mm = Right mm


{-# OPTIONS_HADDOCK hide #-}
module DDC.Source.Tetra.Convert.Base
        ( ConvertM
        , SP

        , module DDC.Type.Universe
        , module DDC.Source.Tetra.Convert.Error
        , module DDC.Data.SourcePos)
where
import DDC.Type.Universe
import DDC.Source.Tetra.Convert.Error
import DDC.Data.SourcePos


type ConvertM a x
        = Either (ErrorConvert a) x


type SP = SourcePos




module DDC.Source.Tetra.Infer.Expand
        ( Config        (..)
        , configDefault
        , expandModule)
where
import DDC.Source.Tetra.Module
import DDC.Source.Tetra.Name
import DDC.Source.Tetra.Exp


-------------------------------------------------------------------------------
data Config a n
        = Config
        { configMakeTypeHole    :: Kind n -> Type n }


configDefault :: Config a Name
configDefault 
        = Config
        { configMakeTypeHole    = \k -> TVar (UPrim NameHole k)}


-------------------------------------------------------------------------------
expandModule :: Config a n -> Module a n -> Module a n
expandModule _config mm
        = mm


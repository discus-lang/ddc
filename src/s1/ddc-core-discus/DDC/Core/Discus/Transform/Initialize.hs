
-- | Add code to initialize the module.
module DDC.Core.Discus.Transform.Initialize
        (initializeModule)
where
import DDC.Core.Module
import qualified DDC.Core.Salt.Runtime          as A
import qualified DDC.Core.Discus                as D


initializeModule
        :: A.Config
        -> Module a D.Name
        -> Module a D.Name

initializeModule _config mm
        = mm
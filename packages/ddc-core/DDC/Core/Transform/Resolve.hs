
module DDC.Core.Transform.Resolve
        ( resolveNamesInModule )
where
import DDC.Core.Collect.Support
import DDC.Core.Module
import Data.Map                 (Map)


-- | For all the names that are free in this module, if there is a corresponding 
--   export in one of the modules in the given map, then add the appropriate
--   import definition.
resolveNamesInModule 
        :: Ord n
        => Map ModuleName (Module b n)
        -> Module a n
        -> Module a n

resolveNamesInModule _deps mm
 = mm



module DDC.Core.Tetra.Transform.Curry
        (curryModule)
where
import DDC.Core.Tetra
import DDC.Core.Module


-- | Insert primitives to manage higher order functions in a module.
curryModule :: Module a Name -> Module a Name
curryModule mm
        = mm



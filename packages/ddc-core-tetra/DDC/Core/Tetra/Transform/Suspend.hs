
module DDC.Core.Tetra.Transform.Suspend
        (suspendModule)
where
import DDC.Core.Tetra
import DDC.Core.Annot.AnTEC
import DDC.Core.Module


-- | Wrap suspended expressions in lambdas.
-- 
--   Given:  let s = box (f ...)
--           in  ... g (run s) ... h (run s) ...
--
--   The semantics of the 'box' construct are that evaluation of the body
--   should be suspended. The uses of 'run' later in the program then cause
--   the expression to be evaluated.
--
--   This transform eta-expands suspended expressions like so:
--
--           let s = \(_ : Unit). box (f ...)
--           in  ... g (run (s ())) ... h (run (s ())) ...
--
--   After performing this transform the body of the 'box' is suspended
--   by virtue of the outer lambda. We can then convert the program
--   directly to Core-Salt without needing to produce any extra
--   code for run/box.
--
suspendModule 
        :: Module (AnTEC a Name) Name -> Module (AnTEC a Name) Name

suspendModule m = m

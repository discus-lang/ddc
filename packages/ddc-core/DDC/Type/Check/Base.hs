
module DDC.Type.Check.Base
        ( CheckM (..)
        , newExists
        , newPos

        , throw

        , module DDC.Type.Check.Context
        , module DDC.Type.Check.Error
        , module DDC.Type.Predicates
        , module DDC.Type.Compounds
        , module DDC.Type.Equiv
        , module DDC.Type.Exp
        , module DDC.Base.Pretty)
where
import DDC.Type.Check.Context
import DDC.Type.Check.Error
import DDC.Type.Predicates
import DDC.Type.Compounds
import DDC.Type.Equiv
import DDC.Type.Exp
import DDC.Base.Pretty
import qualified DDC.Control.Monad.Check as G
import DDC.Control.Monad.Check           (throw)


-- | The type checker monad.
type CheckM n   = G.CheckM (Int, Int) (Error n)


-- | Allocate a new existential of sort Comp.
--   Kind inference is only useful for type variables of kind Comp, 
--   because we don't write functions that have polymorphic witness
--   type variables.
newExists :: CheckM n (Exists n)
newExists 
 = do   (ix, pos)       <- G.get
        G.put (ix + 1, pos)
        return  (Exists ix sComp)


-- Allocate a new context stack position.
newPos  :: CheckM n Pos
newPos
 = do   (ix, pos)       <- G.get
        G.put (ix, pos + 1)
        return  (Pos pos)



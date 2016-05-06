
module DDC.Type.Check.Base
        ( CheckM
        , newExists
        , newPos
        , applyContext
        , applySolved

        , throw

        , module DDC.Type.Check.Context
        , module DDC.Type.Check.Error
        , module DDC.Type.Exp.Simple
        , module DDC.Base.Pretty)
where
import DDC.Type.Check.Context
import DDC.Type.Check.Error
import DDC.Type.Exp.Simple
import DDC.Base.Pretty
import DDC.Control.Monad.Check           (throw)
import qualified Data.Set               as Set
import qualified DDC.Control.Monad.Check as G


-- | The type checker monad.
type CheckM n   = G.CheckM (Int, Int) (Error n)


-- | Allocate a new existential of sort Comp.
--   Kind inference is only useful for type variables of kind Comp, 
--   because we don't write functions that have polymorphic witness
--   type variables.
newExists :: Sort n -> CheckM n (Exists n)
newExists s
 = do   (ix, pos)       <- G.get
        G.put (ix + 1, pos)
        return  (Exists ix s)


-- Allocate a new context stack position.
newPos  :: CheckM n Pos
newPos
 = do   (ix, pos)       <- G.get
        G.put (ix, pos + 1)
        return  (Pos pos)


-- | Apply the checker context to a type.
applyContext :: Ord n => Context n -> Type n -> CheckM n (Type n)
applyContext ctx tt
 = case applyContextEither ctx Set.empty tt of

        -- We found an infinite path when trying to complete this
        -- substitution. We get back the existential and the type for it.
        Left  (tExt, tBind) 
                -> throw $ ErrorInfinite tExt tBind
        Right t -> return t


-- | Substitute solved constraints into a type.
applySolved :: Ord n => Context n -> Type n -> CheckM n (Type n)
applySolved ctx tt
 = case applySolvedEither ctx Set.empty tt of

        -- We found an infinite path when trying to complete this
        -- substitution. We get back the existential and the type for it.
        Left  (tExt, tBind)
                -> throw $ ErrorInfinite tExt tBind
        Right t -> return t

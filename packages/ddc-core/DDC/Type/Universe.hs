
-- | Determine the universe that something belongs to based on its type.
module DDC.Type.Universe
        ( Universe(..)
        , universeFromType3
        , universeFromType2
        , universeFromType1
        , universeOfType)
where
import DDC.Type.Exp
import DDC.Type.Compounds
import qualified DDC.Type.Sum   as T


-- | These are the universes of the Disciple core language.
data Universe 
        -- | (level 3). The universe of sorts.
        --   Sorts classify kinds.
        = UniverseSort

        -- | (level 2). The universe of kinds.
        --   Kinds classify specifications.
        | UniverseKind

        -- | (level 1). The universe of specifications.
        --   Specifications classify both witnesses and computations.
        --   In the vanilla Haskell world \"specifications\" are known as \"types\", but in DDC we use
        --   the former term because we overload the word \"type\" to refer to kinds and sorts as well.
        | UniverseSpec

        -- | (level 0). The universe of witnesses.
        --   The existence of a witness in the program guarantees some property about how it 
        --   operates at runtime. For example, a witness of constancy of some region guarantees
        --   objects in that region will not be updated.
        | UniverseWitness

        -- | (level 0). The universe of computation.
        --   Data constructors, functions, effects, regions and closures live here.
        --   These are the things that do something useful at runtime.
        | UniverseComp
        deriving (Show, Eq) 


-- | Given the type of the type of the type of some thing (up three levels),
--   yield the universe of the original thing, or `Nothing` it was badly formed.
universeFromType3 :: Type n -> Maybe Universe
universeFromType3 ss
 = case ss of
        TCon (TyConSort SoConProp) -> Just UniverseWitness
        TCon (TyConSort SoConComp) -> Just UniverseComp
        _                          -> Nothing


-- | Given the type of the type of some thing (up two levels),
--   yield the universe of the original thing, or `Nothing` if it was badly formed.
universeFromType2 :: Type n -> Maybe Universe
universeFromType2 tt
 = case tt of
        TVar _                  -> Nothing
        TCon (TyConSort _)      -> Just UniverseSpec

        TCon (TyConKind kc)     
         -> case kc of
                KiConWitness    -> Just UniverseWitness
                KiConData       -> Just UniverseComp
                KiConRegion     -> Just UniverseComp
                KiConEffect     -> Just UniverseComp
                KiConClosure    -> Just UniverseComp
                _               -> Nothing

        TCon (TyConWitness _)   -> Nothing
        TCon (TyConComp  _)     -> Nothing
        TCon (TyConBound _)     -> Nothing
        TForall _ _             -> Nothing
        TApp _ t2               -> universeFromType2 t2
        TSum _                  -> Nothing


-- | Given the type of some thing (up one level),
--   yield the universe of the original thing, or `Nothing` if it was badly formed.
universeFromType1 :: Type n -> Maybe Universe
universeFromType1 tt
 = case tt of
        TVar u                  -> universeFromType2 (typeOfBound u)
        TCon (TyConSort _)      -> Just UniverseKind
        TCon (TyConKind _)      -> Just UniverseSpec
        TCon (TyConWitness _)   -> Just UniverseWitness
        TCon (TyConComp _)      -> Just UniverseComp
        TCon (TyConBound u)     -> universeFromType2 (typeOfBound u)
        TForall _ t2            -> universeFromType1 t2
        TApp _ t2               -> universeFromType1 t2
        TSum _                  -> Nothing


-- | Yield the universe of some type.
universeOfType :: Type n -> Maybe Universe
universeOfType tt
 = case tt of
        TVar u                  -> universeFromType1 (typeOfBound u)
        TCon (TyConSort _)      -> Just UniverseSort
        TCon (TyConKind _)      -> Just UniverseKind
        TCon (TyConWitness _)   -> Just UniverseSpec
        TCon (TyConComp _)      -> Just UniverseSpec
        TCon (TyConBound u)     -> universeFromType1 (typeOfBound u)
        TForall _ t2            -> universeOfType t2
        TApp _ t2               -> universeOfType t2
        TSum ss                 -> universeFromType1 (T.kindOfSum ss)



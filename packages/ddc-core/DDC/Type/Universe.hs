
module DDC.Type.Universe
        ( Universe(..)
        , universeFromType3
        , universeFromType2
        , universeFromType1
        , universeOfType)
where
import DDC.Type.Exp
import DDC.Base.Pretty
import DDC.Type.Env             as Env
import qualified DDC.Type.Sum   as T


-- | Universes of the Disciple Core language.
data Universe 
        -- | (level 3). The universe of sorts.
        --   Sorts classify kinds.
        = UniverseSort

        -- | (level 2). The universe of kinds.
        --   Kinds classify specifications.
        | UniverseKind

        -- | (level 1). The universe of specifications.
        --   Specifications classify both witnesses and data values.
        --   In the vanilla Haskell world \"specifications\" are known as
        --   \"types\", but here we use the former term because we overload
        --   the word \"type\" to refer to kinds and sorts as well.
        | UniverseSpec

        -- | (level 0). The universe of witnesses.
        --   The existence of a witness in the program guarantees that some
        --   property about how it operates at runtime. For example, a witness
        --   of constancy of some region guarantees objects in that region will
        --   not be updated. This is like the @Prop@ universe in constructive
        --   logic.
        | UniverseWitness

        -- | (level 0). The universe of data values.
        --   These are physical data objects that take up space at runtime.
        --   This is like the @Set@ universe in constructive logic, but the 
        --   expressions may diverge or cause side effects.
        | UniverseData
        deriving (Show, Eq) 


instance Pretty Universe where
 ppr u
  = case u of
        UniverseSort    -> text "Sort"
        UniverseKind    -> text "Kind"
        UniverseSpec    -> text "Spec"
        UniverseWitness -> text "Witness"
        UniverseData    -> text "Data"


-- | Given the type of the type of the type of some thing (up three levels),
--   yield the universe of the original thing, or `Nothing` it was badly formed.
universeFromType3 :: Type n -> Maybe Universe
universeFromType3 ss
 = case ss of
        TCon (TyConSort SoConProp) -> Just UniverseWitness
        TCon (TyConSort SoConComp) -> Just UniverseData
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
                KiConData       -> Just UniverseData
                _               -> Nothing

        TCon TyConWitness{}     -> Nothing
        TCon TyConSpec{}        -> Nothing
        TCon TyConBound{}       -> Nothing
        TForall _ _             -> Nothing
        TApp _ t2               -> universeFromType2 t2
        TSum _                  -> Nothing


-- | Given the type of some thing (up one level),
--   yield the universe of the original thing, or `Nothing` if it was badly formed.
universeFromType1 :: Ord n => Env n -> Type n -> Maybe Universe
universeFromType1 kenv tt
 = case tt of
        TVar n
         -> case Env.lookup n kenv of
                Nothing            -> Nothing
                Just k             -> universeFromType2 k

        TCon (TyConSort _)         -> Just UniverseKind
        TCon (TyConKind _)         -> Just UniverseSpec
        TCon (TyConWitness _)      -> Just UniverseWitness
        TCon (TyConSpec TcConFun)  -> Just UniverseData
        TCon (TyConSpec TcConUnit) -> Just UniverseData
        TCon (TyConSpec _)         -> Nothing
        TCon (TyConBound _ k)      -> universeFromType2 k
        TForall b t2               -> universeFromType1 (Env.extend b kenv) t2
        TApp t1 _                  -> universeFromType1 kenv t1
        TSum _                     -> Nothing


-- | Yield the universe of some type.
--
-- @  universeOfType (tBot kEffect) = UniverseSpec
--  universeOfType kRegion        = UniverseKind
-- @
--
universeOfType :: Ord n => Env n -> Type n -> Maybe Universe
universeOfType kenv tt
 = case tt of
        TVar n
         -> case Env.lookup n kenv of
                Nothing         -> Nothing
                Just k          -> universeFromType1 kenv k

        TCon (TyConSort _)      -> Just UniverseSort
        TCon (TyConKind _)      -> Just UniverseKind
        TCon (TyConWitness _)   -> Just UniverseSpec
        TCon (TyConSpec _)      -> Just UniverseSpec
        TCon (TyConBound _ k)   -> universeFromType1 kenv k
        TForall b t2            -> universeOfType (Env.extend b kenv) t2
        TApp _ t2               -> universeOfType kenv t2
        TSum ss                 -> universeFromType1 kenv (T.kindOfSum ss)



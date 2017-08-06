{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Context.Elem
        ( -- * Positions in the context.
          Pos    (..)

          -- * Roles of type variables.
        , Role   (..)

          -- * Existentials.
        , Exists (..)
        , typeOfExists
        , takeExists
        , slurpExists

          -- * Context elements.
        , Elem   (..)
        , takeExistsOfElem)
where
import DDC.Type.Exp.Simple
import DDC.Data.Pretty
import qualified DDC.Type.Sum   as Sum


-- Positions --------------------------------------------------------------------------------------
-- | A position in the type checker context.
--   A position is used to record a particular position in the context stack,
--   so that we can pop elements higher than it.
data Pos
        = Pos !Int
        deriving (Show, Eq)

instance Pretty Pos where
 ppr (Pos p)
        = text "*" <> int p


-- Role -------------------------------------------------------------------------------------------
-- | The role of some type variable.
data Role
        -- | Concrete type variables are region variables that have been introduced
        --   in an enclosing lexical scope. All the capabilities for these will
        --   also be in the context.
        = RoleConcrete

        -- | Abstract type variables are the ones that are bound by type abstraction
        --   Inside the body of a type abstraction we can assume a region supports
        --   any given capability. We only need to worry about if it really does
        --   when we actually run the enclosed computation.
        | RoleAbstract
        deriving (Show, Eq)


instance Pretty Role where
 ppr role
  = case role of
        RoleConcrete    -> text "Concrete"
        RoleAbstract    -> text "Abstract"


-- Exists -----------------------------------------------------------------------------------------
-- | An existential variable.
data Exists n
        = Exists !Int !(Kind n)
        deriving (Show)

instance Eq (Exists n) where
 (==)   (Exists i1 _) (Exists i2 _)     = i1 == i2
 (/=)   (Exists i1 _) (Exists i2 _)     = i1 /= i2


instance Ord (Exists n) where
 compare (Exists i1 _) (Exists i2 _)
        = compare i1 i2


instance Pretty (Exists n) where
 ppr (Exists i _) = text "?" <> ppr i


-- | Wrap an existential variable into a type.
typeOfExists :: Exists n -> Type n
typeOfExists (Exists n k)
        = TCon (TyConExists n k)


-- | Take an Exists from a type.
takeExists :: Type n -> Maybe (Exists n)
takeExists tt
 = case tt of
        TCon (TyConExists n k)  -> Just (Exists n k)
        _                       -> Nothing


-- | Slurp all the existential variables from this type.
slurpExists :: Type n -> [Exists n]
slurpExists tt
 = case tt of
        TCon (TyConExists n k)  -> [Exists n k]
        TCon _                  -> []
        TVar {}                 -> []
        TAbs b xBody            -> slurpExists (typeOfBind b) ++ slurpExists xBody
        TApp t1 t2              -> slurpExists t1 ++ slurpExists t2
        TForall b xBody         -> slurpExists (typeOfBind b) ++ slurpExists xBody
        TSum ts                 -> concatMap slurpExists $ Sum.toList ts


-- Elem -------------------------------------------------------------------------------------------
-- | An element in the type checker context.
data Elem n
        -- | A context position marker.
        = ElemPos        !Pos

        -- | Kind of some variable.
        | ElemKind       !(Bind n) !Role

        -- | Type of some variable.
        | ElemType       !(Bind n)

        -- | Existential variable declaration
        | ElemExistsDecl !(Exists n)

        -- | Existential variable solved to some monotype.
        | ElemExistsEq   !(Exists n) !(Type n)
        deriving (Show, Eq)


instance (Pretty n, Eq n) => Pretty (Elem n) where
 ppr ll
  = case ll of
        ElemPos p
         -> text "P "
         <> ppr p

        ElemKind b role
         -> text "K "
         <> (padL 4 $ ppr (binderOfBind b))
                <+> text ":"
                <+> (ppr $ typeOfBind b)
                <+> text "@" <> ppr role

        ElemType b
         -> text "T "
         <> (padL 4 $ ppr (binderOfBind b))
                <+> text ":"
                <+> (ppr $ typeOfBind b)

        ElemExistsDecl (Exists i k)
         -> text "D "
         <> padL 4 (text "?" <> ppr i) <+> text ":" <+> ppr k

        ElemExistsEq (Exists i k) t
         -> text "E "
         <> padL 4 (text "?" <> ppr i) <+> text ":" <+> ppr k <+> text "=" <+> ppr t


-- | Take the existential from this context element, if there is one.
takeExistsOfElem :: Elem n -> Maybe (Exists n)
takeExistsOfElem ee
 = case ee of
        ElemExistsDecl i        -> Just i
        ElemExistsEq   i _      -> Just i
        _                       -> Nothing

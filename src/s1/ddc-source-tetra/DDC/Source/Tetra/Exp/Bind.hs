{-# OPTIONS_HADDOCK hide #-}

module DDC.Source.Tetra.Exp.Bind
        ( Name
        , Bind          (..)
        , Bound         (..)
        , takeBoundOfBind

        , DaConBind     (..)
        , DaConBound    (..)
        , TyConBind     (..)
        , TyConBound    (..))
where
import DDC.Source.Tetra.Prim.Base
import Data.Text                (Text)

type Name = Text

-- | Binding occurrence of a variable.
data Bind
        = BNone
        | BAnon
        | BName !Text
        deriving (Eq, Ord, Show)


-- | Bound occurrence of a variable.
data Bound
        -- A named variable.
        = UName !Text

        -- A deBruijn idex.
        | UIx   !Int

        -- A hole that we want the type checker to fill in.
        | UHole
        deriving (Eq, Ord, Show)


-- | Take the corresponding `Bound` of a `Bind`, if there is one.
takeBoundOfBind :: Bind -> Maybe Bound
takeBoundOfBind bb
 = case bb of
        BNone    -> Nothing
        BAnon    -> Just $ UIx 0
        BName tx -> Just $ UName tx


-- | Binding occurrence of a data constructor.
data DaConBind
        = DaConBindName  Text
        deriving (Eq, Ord, Show)


-- | Bound occurrences of a data constructor.
data DaConBound
        = DaConBoundName Text
        | DaConBoundLit  PrimLit
        deriving (Eq, Ord, Show)


-- | Binding occurrence of a type constructor.
data TyConBind
        = TyConBindName  Text
        deriving (Eq, Ord, Show)


-- | Bound occurrence of a type constructor.
data TyConBound
        = TyConBoundName Text
        deriving (Eq, Ord, Show)


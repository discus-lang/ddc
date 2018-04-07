{-# OPTIONS_HADDOCK hide #-}

module DDC.Source.Discus.Exp.Bind
        ( Name
        , Bind          (..)
        , isBAnon

        , Bound         (..)
        , takeBoundOfBind

        , TyConBind     (..)
        , TyConBound    (..)

        , withBindings
        , withBinding)
where
import Data.Text                (Text)

type Name = Text

-- | Binding occurrence of a variable.
data Bind
        = BNone
        | BAnon
        | BName !Text
        deriving (Eq, Ord, Show)

isBAnon :: Bind -> Bool
isBAnon BAnon   = True
isBAnon _       = False


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


-- | Binding occurrence of a type constructor.
data TyConBind
        = TyConBindName  !Text
        deriving (Eq, Ord, Show)


-- | Bound occurrence of a type constructor.
data TyConBound
        = TyConBoundName !Text
        deriving (Eq, Ord, Show)


withBindings :: Int -> ([Bind] -> [Bound] -> a) -> a
withBindings n f
  = let bs      = replicate n BAnon
        us      = reverse [UIx i | i <- [0..(n - 1)]]
    in  f bs us


withBinding :: (Bind -> Bound -> a) -> a
withBinding f = f BAnon (UIx 0)


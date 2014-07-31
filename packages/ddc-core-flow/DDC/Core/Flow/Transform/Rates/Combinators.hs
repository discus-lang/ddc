-- | Converting DDC expressions to and from Combinator Normal Form.
module DDC.Core.Flow.Transform.Rates.Combinators
        ( Fun(..), Bind(..), ABind(..), SBind(..), Seed(..)
        , Program(..)
        , CName(..)
        , lookupA, lookupS, lookupB
        , envOfBind
        , freeOfBind, cnameOfEither, cnameOfBind
        ) where
import DDC.Base.Pretty
import DDC.Core.Flow.Exp (ExpF)
import qualified Control.Applicative as A

-----------------------------------
-- = Combinator normal form.


-- | Worker function. May only reference scalars in the environment, not arrays.
-- Takes the expression of the function, and a list of the free scalars that are referenced inside it.
-- The expression must be a function from scalar to scalar.
data Fun s a
 = Fun ExpF [s]
   deriving Show

-- | Array, scalar and external bindings.
-- Array bindings are those whose value is an array, such as map, filter.
-- Scalar bindings have scalar values, currently only fold.
-- External expressions are those that cannot be converted to primitive combinators.
-- The they take a single expression that computes all outputs, with the list of free scalar and array inputs.
data Bind s a
 = ABind a (ABind s a)
 | SBind s (SBind s a)
 | Ext
   { _beOuts :: ([s], [a])
   , _beExp  :: ExpF
   , _beIns  :: ([s], [a])
   }
   deriving Show

-- | An array-valued binding.
data ABind s a
 -- | map_n     :: (a_1 ... a_n -> b) -> Array a_1 ... Array a_n -> Array b
 = MapN       (Fun s a) [a]
 -- | filter    :: (a -> Bool)        -> Array a                 -> Array a
 | Filter     (Fun s a)  a
 -- | generate  ::  Nat               -> (Nat -> a)              -> Array a
 | Generate s (Fun s a) 
 -- | gather    ::  Array a           -> Array Nat               -> Array a
 | Gather                a a
 -- | cross     ::  Array a           -> Array b                 -> Array (a, b)
 | Cross                 a a
   deriving Show

-- | Seed / initial value for a fold.
-- This can either be a literal such as "0", or a named scalar reference
data Seed s a
 = Seed ExpF (Maybe s)
   deriving Show

-- | A scalar-valued binding
data SBind s a
 -- | fold      :: (a -> a -> a) -> a -> Array a                 -> a
 = Fold       (Fun s a) (Seed s a) a
   deriving Show

-- | An entire program/function to find a fusion clustering for
data Program s a
 = Program
   { _ins   :: ([s], [a])
   , _binds :: [Bind s a]
   , _outs  :: ([s], [a])
   }
   deriving Show

-- | Name of a combinator.
-- This will also be the name of the corresponding node of the graph.
data CName s a
 = NameScalar s
 | NameArray a
 | NameExt ([s],[a])
 deriving (Eq, Ord, Show)


lookupA :: Eq a => Program s a -> a -> Maybe (ABind s a)
lookupA p a
 = go $ _binds p
 where
  go [] = Nothing
  go (ABind a' b : _)
   | a == a'
   = Just b
  go (_ : bs)
   = go bs

lookupS :: Eq s => Program s a -> s -> Maybe (SBind s a)
lookupS p s
 = go $ _binds p
 where
  go [] = Nothing
  go (SBind s' b : _)
   | s == s'
   = Just b
  go (_ : bs)
   = go bs


lookupB :: (Eq s, Eq a) => Program s a -> Either s a -> Maybe (Either (SBind s a) (ABind s a))
lookupB p (Left  s) = Left  A.<$> lookupS p s
lookupB p (Right a) = Right A.<$> lookupA p a


envOfBind :: Bind s a -> ([s], [a])
envOfBind (SBind s _) = ([s], [])
envOfBind (ABind a _) = ([] , [a])
envOfBind (Ext outs _ _) = outs


cnameOfBind :: Bind s a -> CName s a
cnameOfBind (SBind s _)    = NameScalar s
cnameOfBind (ABind a _)    = NameArray  a
cnameOfBind (Ext outs _ _) = NameExt    outs

freeOfBind :: Bind s a -> [Either s a]
freeOfBind b
 = case b of
   SBind _ (Fold fun i a)
    -> ffun fun ++ fseed i ++ [fa a]
   ABind _ (MapN fun as)
    -> ffun fun ++ map fa as
   ABind _ (Filter fun a)
    -> ffun fun ++ [fa a]
   ABind _ (Generate s fun)
    -> ffun fun ++ [fs s]
   ABind _ (Gather x y)
    -> [fa x, fa y]
   ABind _ (Cross x y)
    -> [fa x, fa y]
   Ext _ _ (inS,inA)
    -> map fs inS ++ map fa inA
 where
  ffun  (Fun _ f)               = map fs f
  fseed (Seed _ Nothing)        = []
  fseed (Seed _ (Just s))       = [Left s]
  fs                            = Left
  fa                            = Right

-- | get the name of the binding that provides this value.
-- this is made trickier because externals can bind multiple.
cnameOfEither :: (Eq s, Eq a) => Program s a -> Either s a -> Maybe (CName s a)
cnameOfEither prog esa
 | Left  s <- esa
 , Just  _ <- lookupS prog s
 = Just $ NameScalar s
 | Right a <- esa
 , Just  _ <- lookupA prog a
 = Just $ NameArray  a
 -- Otherwise, try to find an external
 | otherwise
 = go (_binds prog)
 where
  go [] = Nothing
  go (Ext (ss,as) _ _ : _)
   | Left  s <- esa
   , s `elem` ss
   = Just $ NameExt (ss,as)
   | Right a <- esa
   , a `elem` as
   = Just $ NameExt (ss,as)
  go (_ : bs)
   = go bs


-----------------------------------
-- == Pretty printing
-- This is just the notation I used in the prototype.

instance (Pretty s, Pretty a) => Pretty (Fun s a) where
 ppr (Fun _ ss)
  = encloseSep lbrace rbrace space
  $ map ppr ss

instance (Pretty s, Pretty a) => Pretty (Seed s a) where
 ppr (Seed _ Nothing)
  = text "-"
 ppr (Seed _ (Just s))
  = ppr s

instance (Pretty s, Pretty a) => Pretty (Bind s a) where
 ppr (SBind n (Fold f i a))
  = bind (ppr n) "reduce" (ppr f <+> ppr i <+> ppr a)

 ppr (ABind n (MapN f as))
  = bind (ppr n) "mapN"   (ppr f <+> hsep (map ppr as))

 ppr (ABind n (Filter f a))
  = bind (ppr n) "filter" (ppr f <+> ppr a)

 ppr (ABind n (Gather a b))
  = bind (ppr n) "gather" (ppr a <+> ppr b)

 ppr (ABind n (Generate sz f))
  = bind (ppr n) "generate" (ppr sz <+> ppr f)

 ppr (ABind n (Cross a b))
  = bind (ppr n) "cross"    (ppr a <+> ppr b)

 ppr (Ext outs _ ins)
  = bind (binds outs) "external" (binds ins)
  where
   binds (ss,as)
    = encloseSep lbrace rbrace space (map ppr ss) <+> hcat (map ppr as)

bind :: Doc -> String -> Doc -> Doc
bind nm com args
 = nm <+> nest 4 (equals <+> text com <+> args)

instance (Pretty s, Pretty a) => Pretty (Program s a) where
 ppr (Program ins binds outs)
  = params <$> vcat (map ppr binds) <$> returns
  where
   params
    =   vcat (map (\i -> text "param scalar" <+> ppr i) (fst ins))
    <$> vcat (map (\i -> text "param array"  <+> ppr i) (snd ins))

   returns
    =   vcat (map (\i -> text "return"       <+> ppr i) (fst outs))
    <$> vcat (map (\i -> text "return"       <+> ppr i) (snd outs))

instance (Pretty s, Pretty a) => Pretty (CName s a) where
 ppr (NameScalar s) = text "{" <> ppr s <> text "}"
 ppr (NameArray  a) =             ppr a
 ppr (NameExt (ss,aa)) = text "ext{" <> ppr ss <+> ppr aa <> text "}"

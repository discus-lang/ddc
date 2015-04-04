-- | Converting DDC expressions to and from Combinator Normal Form.
module DDC.Core.Flow.Transform.Rates.Combinators
        ( Fun(..), Bind(..), ABind(..), SBind(..), Scalar(..)
        , Program(..)
        , CName(..)
        , lookupA, lookupS, lookupB
        , envOfBind
        , freeOfBind, cnameOfBind
        , outputsOfCluster, inputsOfCluster
        , seriesInputsOfCluster
        ) 
where
import DDC.Base.Pretty
import DDC.Core.Flow.Exp (ExpF)
import Data.Maybe (catMaybes)
import Data.List  (nub)
import Prelude                  hiding ((<$>))

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
   { _beOut  :: CName s a
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
 | Generate (Scalar s a) (Fun s a) 
 -- | gather    ::  Array a           -> Array Nat               -> Array a
 | Gather                a a
 -- | cross     ::  Array a           -> Array b                 -> Array (a, b)
 | Cross                 a a
   deriving Show

-- | Scalars can either be a literal such as "0", or a named scalar reference.
-- If it's not a named scalar reference, we need to keep the expression so we can reconstruct it later.
-- (We do not have array literals, so this is only necessary for scalars)
data Scalar s a
 = Scalar ExpF (Maybe s)
   deriving Show

-- | A scalar-valued binding
data SBind s a
 -- | fold      :: (a -> a -> a) -> a -> Array a                 -> a
 = Fold       (Fun s a) (Scalar s a) a
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


lookupB :: (Eq s, Eq a) => Program s a -> CName s a -> Maybe (Bind s a)
lookupB p nm = go (_binds p)
 where
  go [] = Nothing
  go (b@(ABind a _) : _)
   | NameArray a' <- nm
   , a == a'
   = Just b
  go (b@(SBind s _) : _)
   | NameScalar s' <- nm
   , s == s'
   = Just b
  go (b@(Ext nm' _ _) : _)
   | nm == nm'
   = Just $ b
  go (_ : bs)
   = go bs



envOfBind :: Bind s a -> ([s], [a])
envOfBind (SBind s _)              = ([s], [])
envOfBind (ABind a _)              = ([], [a])
envOfBind (Ext (NameScalar s) _ _) = ([s], [])
envOfBind (Ext (NameArray  a) _ _) = ([], [a])


cnameOfBind :: Bind s a -> CName s a
cnameOfBind (SBind s _) = NameScalar s
cnameOfBind (ABind a _) = NameArray  a
cnameOfBind (Ext n _ _) = n

freeOfBind :: Bind s a -> [CName s a]
freeOfBind b
 = case b of
   SBind _ (Fold fun i a)
    -> ffun fun ++ fscalar i ++ [fa a]
   ABind _ (MapN fun as)
    -> ffun fun ++ map fa as
   ABind _ (Filter fun a)
    -> ffun fun ++ [fa a]
   ABind _ (Generate s fun)
    -> ffun fun ++ fscalar s
   ABind _ (Gather x y)
    -> [fa x, fa y]
   ABind _ (Cross x y)
    -> [fa x, fa y]
   Ext _ _ (inS,inA)
    -> map fs inS ++ map fa inA
 where
  ffun  (Fun _ f)               = map fs f
  fscalar (Scalar _ Nothing)        = []
  fscalar (Scalar _ (Just s))       = [NameScalar s]
  fs                            = NameScalar
  fa                            = NameArray

-- | Get inputs that must be converted to series or rate vectors
seriesInputOfBind :: Bind s a -> [a]
seriesInputOfBind b
 = case b of
   SBind _ (Fold _fun _i a)
    -> [a]
   ABind _ (MapN _fun as)
    -> as
   ABind _ (Filter _fun a)
    -> [a]
   ABind _ (Generate _s _fun)
    -> []
   ABind _ (Gather v ix)
   -- Only the indices array is consumed series-wise.
   -- The vector is random access.
    -> [v, ix]
   -- Cross product's first is consumed in series, but second is consumed multiple times
   ABind _ (Cross x y)
    -> [x, y]
   -- Externals do not require series inputs.
   Ext _ _ (_inS,_inA)
    -> []



-- | For a given program and list of nodes that will be clustered together,
-- find a list of the nodes that are used afterwards.
-- Only these nodes must be made manifest.
-- The output nodes is a subset of the input cluster nodes.
outputsOfCluster :: (Eq s, Eq a) => Program s a -> [CName s a] -> [CName s a]
outputsOfCluster prog cluster
       -- Get all bindings in the program that aren't in this cluster
 = let notin   = filter (not . flip elem cluster . cnameOfBind) (_binds prog)
       -- And find their free variables
       frees   = concatMap freeOfBind          notin

       -- Convert the returns of the program to CNames
       (ss,as) = _outs prog
       pouts   = map NameScalar ss ++ map NameArray as

       -- We want to look in both returns and free variables of bindings
       alls    = frees ++ pouts
       -- Now search through and find those in the cluster
       found   = filter (flip elem cluster) alls
   in  nub $ found


-- | For a given program and list of nodes that will be clustered together,
-- find a list of the nodes that are used as inputs.
-- The input nodes will not mention any of the cluster nodes.
inputsOfCluster :: (Eq s, Eq a) => Program s a -> [CName s a] -> [CName s a]
inputsOfCluster prog cluster
       -- Get bindings of clusters
 = let binds   = catMaybes
               $ map (lookupB prog) cluster
       -- And find the free variables
       frees   = concatMap freeOfBind          binds

       -- Ignore the ones in the cluster
       found   = filter (not . flip elem cluster) frees
   in  nub $ found

-- | For a given program and list of nodes that will be clustered together,
-- find a list of the inputs that need to be converted to series.
-- If the cluster is correct, these should all be the same size.
seriesInputsOfCluster :: (Eq s, Eq a) => Program s a -> [CName s a] -> [a]
seriesInputsOfCluster prog cluster
       -- Get bindings of clusters
 = let binds   = catMaybes
               $ map (lookupB prog) cluster
       -- And find the free variables
       frees   = concatMap seriesInputOfBind      binds

       -- Ignore the ones in the cluster
       found   = filter (not . flip elem cluster . NameArray) frees
   in  nub $ found


-----------------------------------
-- == Pretty printing
-- This is just the notation I used in the prototype.

instance (Pretty s, Pretty a) => Pretty (Fun s a) where
 ppr (Fun _ ss)
  = encloseSep lbrace rbrace space
  $ map ppr ss

instance (Pretty s, Pretty a) => Pretty (Scalar s a) where
 ppr (Scalar _ Nothing)
  = text "-"
 ppr (Scalar _ (Just s))
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

 ppr (Ext out _ ins)
  = bind (ppr out) "external" (binds ins)
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

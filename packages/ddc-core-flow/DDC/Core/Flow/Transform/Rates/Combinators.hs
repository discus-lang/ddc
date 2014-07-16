-- | Converting DDC expressions to and from Combinator Normal Form.
module DDC.Core.Flow.Transform.Rates.Combinators
        ( Fun(..), Bind(..), ABind(..), SBind(..)
        , Program(..)
        ) where
import DDC.Core.Flow.Exp (ExpF)

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

-- A scalar-valued binding
data SBind s a
 -- | fold      :: (a -> a -> a) -> a -> Array a                 -> a
 = Fold       (Fun s a)  a
   deriving Show

-- | An entire program/function to find a fusion clustering for
data Program s a
 = Program
   { _ins   :: ([s], [a])
   , _binds :: [Bind s a]
   , _outs  :: ([s], [a])
   }
   deriving Show


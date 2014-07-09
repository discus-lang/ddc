-- | Converting DDC expressions to and from Combinator Normal Form.
module DDC.Core.Flow.Transform.Rates.Combinators
        ( Fun(..), Bind(..), ABind(..), SBind(..)
        , Program(..)
        , cnfOfExp
        ) where
import DDC.Core.Collect
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Transform.Rates.Fail

import           Control.Applicative
import           Control.Monad

-----------------------------------
-- = Combinator normal form.


-- | Worker function. May only reference scalars in the environment, not arrays.
-- Takes the expression of the function, and a list of the free scalars that are referenced inside it.
-- The expression must be a function from scalar to scalar.
data Fun s a
 = Fun ExpF [s]

-- | Array, scalar and external bindings.
-- Array bindings are those whose value is an array, such as map, filter.
-- Scalar bindings have scalar values, currently only fold.
-- External expressions are those that cannot be converted to primitive combinators.
-- The they take a single expression that computes all outputs, with the list of free scalar and array inputs.
data Bind s a
 = ABind a (ABind s a)
 | SBind s (SBind s a)
 | Ext
   { _beOuts = ([s], [a])
   , _beExp  = ExpF
   , _beIns  = ([s], [a])
   }

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

-- A scalar-valued binding
data SBind s a
 -- | fold      :: (a -> a -> a) -> a -> Array a                 -> a
 = Fold       (Fun s a)  a

-- | An entire program/function to find a fusion clustering for
data Program s a
 = Program
   { _ins   = ([s], [a])
   , _binds = [Bind s a]
   , _outs  = ([s], [a])
   }
   deriving Show


-----------------------------------
-- = Conversion from ExpF to CNF.
--
-- | Convert a given expression function to CNF.
-- For this to succeed, the function must:
--      - be in A-normal form
--      - all bindings are named, not de Bruijn indices
--      - names must be unique
--      - no recursive bindings
--      - no @letregion@s
--
-- If it succeeds, it should be true that
-- >>> expOfCnf . right . cnfOfExp = id
-- at least semantically, if not syntactically
-- 
cnfOfExp :: ExpF -> Either ConversionError (Program Name Name)
cnfOfExp x
 = do   -- Peel off the lambdas
        let (lams, body)   = takeXLamFlags_safe fun
        -- Assuming the body is already in a-normal form.
            (lets, xx)     = splitXLets         body
        -- TODO check that xx is a-normal?

        -- Split into name and values and warn for recursive bindings
        binds             <- takeLets           lets

        let names = map fst binds
        -- Make sure names are unique
        when (length names /= length (nub names)) $
          Left FailNamesNotUnique

        -- For each value-level lambda binder, decide whether it's scalar or vector based on type
        let inputs  = mconcat $ map getInput lams
            getInput (False, BName n ty)
             | isTypeArray ty
             = ([],[n])
             | otherwise
             = ([n],[])
            getInput (_,_) = ([],[])

        -- For each binding, classify it as either array, scalar or external.
        --
        -- We must be careful about creating externals, though: if a binding is just a
        -- worker function, we don't really need that as an external.
        -- However, if we assume that all scalars will be fusion-preventing (they currently are),
        -- then creating externals for these will not affect scheduling.
        -- But what of worker functions referencing vectors? It becomes harder to outlaw if the
        -- worker function is not inlined into the combinator binding.
        -- Tuples are another potential problem here: looking at the tuple's type, it would not be
        -- an array binding.

        -- TODO
        return (Program inputs [] ([],[]))

-- | Check if type is an array type, so we know whether variables are scalar or array.
-- This is perhaps a crude way to test, as what if the result of a fold is actually a vector?
-- Well, let's not worry about that right now.
isTypeArray :: TypeF -> Bool
isTypeArray = isVectorType


-- | Peel the lambdas off, or leave it alone if there are none
takeXLamFlags_safe x
 | Just (binds, body) <- takeXLamFlags x
 = (binds, body)
 | otherwise
 = ([],    x)


-- | Split into name and values and error for outlawed bindings
takeLets :: [LetsF] -> Either ConversionError [(Name, ExpF)]
takeLets lets
 = mapM get lets
 where
  get (LLet (BName n _) x) = return (n,x)
  get (LLet (BNone _)   _) = Left   FailNoAnonAllowed
  get (LLet (BAnon _)   _) = Left   FailNoDeBruijnAllowed
  get (LRec        _     ) = Left   FailRecursiveBindings
  get (LPrivate _ _ _)     = Left   FailLetRegionNotHandled
  get (LWithRegion _     ) = Left   FailLetRegionNotHandled


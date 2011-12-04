
-- | Type substitution.
module DDC.Type.Transform.SubstituteT
        (SubstituteT(..))
where
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Collect.Free
import DDC.Type.Transform.LiftT
import Data.Maybe
import Data.List
import qualified DDC.Type.Sum   as Sum
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set
import Data.Set                 (Set)


class SubstituteT (c :: * -> *) where
 -- | Substitute a type into some thing.
 --   In the target, if we find a named binder that would capture a free variable
 --   in the type to substitute, then we rewrite that binder to anonymous form,
 --   avoiding the capture.
 substituteWithT
        :: forall n. Ord n
        => Bound n              -- ^ Bound variable that we're subsituting into.
        -> Type n               -- ^ Type to substitute.
        -> Set  n               -- ^ Names of free varaibles in the type to substitute.
        -> [Bind n]             -- ^ Anonymous binders that we've entered under, and named
                                --   binders that we're rewriting.
        -> Int                  -- ^ Number of BAnons in the stack.
        -> Int                  -- ^ Number of BNames in the stack.
        -> c n -> c n

 -- | Wrapper for `substituteWithT` that determines the set of free names in the
 --   type being substituted, and starts with an empty binder stack.
 substituteT :: (SubstituteT c, Ord n) => Bound n -> Type n -> c n -> c n
 substituteT u t x
  = let -- Determine the free names in the type we're subsituting.
        -- We'll need to rename binders with the same names as these
        freeNames       = Set.fromList
                        $ mapMaybe takeNameOfBound 
                        $ Set.toList 
                        $ free Env.empty t
 
   in   substituteWithT u t freeNames [] 0 0 x


instance SubstituteT Bind where
 substituteWithT u fvs t stack dAnon dName bb
  = let k'      = substituteWithT u fvs t stack dAnon dName $ typeOfBind bb
    in  replaceTypeOfBind k' bb
  
        
instance SubstituteT Type where
 substituteWithT u t fns stack dAnon dName tt
  = let down    = substituteWithT u t fns stack dAnon dName
    in  case tt of
         TCon{}          -> tt

         TApp t1 t2      -> TApp (down t1) (down t2)
         TSum ss         -> TSum (down ss)
         TBot k          -> TBot (down k)

         TForall b tBody
          -> let -- Substitute into the annotation on the binder.
                bSub    = down b

                (b', stack', dAnon', dName')

                 -- Push anonymous binder on the stack.
                 | BAnon t      <- bSub
                 = (BAnon t, BAnon t   : stack, dAnon + 1, dName)

                 -- If this binder would capture names in the type that we're
                 -- substituting then rewrite it to an anonymous one.
                 | BName n t     <- bSub
                 , Set.member n fns
                 = (BAnon t, BName n t : stack, dAnon,     dName + 1)
         
                 | otherwise
                 = (bSub,    stack,             dAnon,     dName)

                tBody'  = substituteWithT u t fns stack' dAnon' dName' tBody

             in  TForall b' tBody'


         TVar u'
          -- Bound name matches the one that we're substituting for.
          | UName n1 _   <- u
          , UName n2 _   <- u'
          , n1 == n2
          -> liftT (dAnon + dName) t

          -- The Bind for this name was rewritten to avoid variable capture,
          -- so we also have to update the bound occurrence.
          | UName n t    <- u'
          , Just ix      <- findIndex (boundMatchesBind u') stack
          -> TVar $ UIx ix t

          -- Bound index matches the one that we're substituting for.
          | UIx i1 _     <- u
          , UIx i2 _     <- u'
          , i1 + dAnon == i2 
          -> liftT (dAnon + dName) t

          -- Bound index doesn't match, but lower this index by one to account
          -- for the removal of the outer binder.
          | UIx  i2 t    <- u'
          , cutOffset    <- case u of
                                 UIx{}   -> 1
                                 _       -> 0
          -> TVar $ UIx (i2 + dName - cutOffset) t

          -- Some name that didn't match.
          | otherwise
          -> tt


instance SubstituteT TypeSum where
 substituteWithT u n fns stack dAnon dName ss
  = let k       = substituteWithT u n fns stack dAnon dName 
                $ Sum.kindOfSum ss
    in  Sum.fromList k 
                $ map (substituteWithT u n fns stack dAnon dName)
                $ Sum.toList ss



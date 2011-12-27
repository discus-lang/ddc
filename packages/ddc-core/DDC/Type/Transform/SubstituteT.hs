
-- | Type substitution.
module DDC.Type.Transform.SubstituteT
        ( SubstituteT(..)
        , substituteT
        , substituteTs

        , BindStack(..)
        , pushBind
        , pushBinds
        , substBound)
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
        -> BindStack n          -- ^ Bind stack.
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

       stack           = BindStack [] 0 0
 
  in   substituteWithT u t freeNames stack x


-- | Wrapper for `substituteT` to substitute multiple things.
substituteTs :: (SubstituteT c, Ord n) => [(Bound n, Type n)] -> c n -> c n
substituteTs bts x
        = foldr (uncurry substituteT) x bts


-- Instances ------------------------------------------------------------------
instance SubstituteT Bind where
 substituteWithT u fvs t stack bb
  = let k'      = substituteWithT u fvs t stack $ typeOfBind bb
    in  replaceTypeOfBind k' bb
 
 
instance SubstituteT Type where
 substituteWithT u t fns stack tt
  = let down    = substituteWithT u t fns stack
    in  case tt of
         TCon{}          -> tt

         TApp t1 t2      -> TApp (down t1) (down t2)
         TSum ss         -> TSum (down ss)

         TForall b tBody
          -> let -- Substitute into the annotation on the binder.
                 bSub            = down b

                 -- Push bind onto stack, and anonymise to avoid capture if needed
                 (stack', b')    = pushBind fns stack bSub
                
                 -- Substitute into body.
                 tBody'          = substituteWithT u t fns stack' tBody

             in  TForall b' tBody'

         TVar u'
          -> case substBound stack u u' of
                Left  u'' -> TVar u''
                Right n   -> liftT n t
                

instance SubstituteT TypeSum where
 substituteWithT u n fns stack ss
  = let k       = substituteWithT u n fns stack
                $ Sum.kindOfSum ss
    in  Sum.fromList k 
                $ map (substituteWithT u n fns stack)
                $ Sum.toList ss


-------------------------------------------------------------------------------
-- | Stack of anonymous binders that we've entered under, 
--   and named binders that we're rewriting.
data BindStack n
        = BindStack
        { stackBinds    :: [Bind n]
        , stackAnons    :: Int
        , stackNamed    :: Int }

-- | Push a bind onto a bind stack, 
--   anonymising it if need be to avoid variable capture.
pushBind
        :: Ord n
        => Set n                  -- ^ Names free in the thing we're substiuting.
        -> BindStack n            -- ^ Current bind stack.
        -> Bind n                 -- ^ Bind to push.
        -> (BindStack n, Bind n)  -- ^ New stack and possibly anonymised bind.

pushBind fns bs@(BindStack stack dAnon dName) bb
 = case bb of
        -- Push anonymous bind on stack.
        BAnon t                 
         -> ( BindStack (BAnon t   : stack) (dAnon + 1) dName
            , BAnon t)
            
        -- This binder would capture names in the thing that we're substituting,
        -- to rewrite it to an anonymous one.
        BName n t
         | Set.member n fns     
         -> ( BindStack (BName n t : stack) dAnon       (dName + 1)
            , BAnon t)

        -- Binder was a wildcard or non-capturing name.
        _ -> (bs, bb)


-- | Push several binds onto the bind stack,
--   anonymysing them if need be to avoid variable capture.
pushBinds :: Ord n => Set n -> BindStack n -> [Bind n]  -> (BindStack n, [Bind n])
pushBinds fns stack bs
        = mapAccumL (pushBind fns) stack bs


-- | Compare a `Bound` against the one we're substituting for.
substBound
        :: Ord n
        => BindStack n      -- ^ Current Bind stack during substitution.
        -> Bound n          -- ^ Bound we're substituting for.
        -> Bound n          -- ^ Bound we're looking at now.
        -> Either 
                (Bound n)   --   Bound doesn't match, but rewite it to this one.
                Int         --   Bound matches, drop the thing being substituted and 
                            --   and lift indices this many steps.

substBound (BindStack binds dAnon dName) u u'
        -- Bound name matches the one that we're substituting for.
        | UName n1 _   <- u
        , UName n2 _   <- u'
        , n1 == n2
        = Right (dAnon + dName)

        -- The Bind for this name was rewritten to avoid variable capture,
        -- so we also have to update the bound occurrence.
        | UName _ t     <- u'
        , Just ix       <- findIndex (boundMatchesBind u') binds
        = Left $ UIx ix t

        -- Bound index matches the one that we're substituting for.
        | UIx  i1 _     <- u
        , UIx  i2 _     <- u'
        , i1 + dAnon == i2 
        = Right (dAnon + dName)

        -- Bound index doesn't match, but lower this index by one to account
        -- for the removal of the outer binder.
        | UIx  i2 t     <- u'
        , i2 > dAnon
        , cutOffset     <- case u of
                                UIx{}   -> 1
                                _       -> 0
        = Left $ UIx (i2 + dName - cutOffset) t

        -- Some name that didn't match.
        | otherwise
        = Left u'

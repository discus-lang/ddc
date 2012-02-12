
-- | Utils for rewriting variables to anonymous form to avoid variable capture.
module DDC.Type.Rewrite
        ( Rewrite(..)
        , Sub(..)
        , BindStack(..)
        , addBind
        , pushBind
        , pushBinds
        , substBound

        , bindT, bindX, bindXs
        , use1,  use0)
where
import DDC.Core.Exp
import DDC.Type.Compounds
import Data.List
import Data.Set                         (Set)
import qualified DDC.Type.Sum           as Sum
import qualified Data.Set               as Set


-- | Substitution state.
--   Keeps track of the binders in the environment that have been rewrittten
--   to avoid variable capture or spec conflict.
data Sub n
        = Sub
        { -- | Bound variable that we're substituting for.
          subBound      :: Bound n

          -- | We've decended past a binder that shadows the one that we're
          --   substituting for.  This can only happen for level-0 names.
        , subShadow0    :: Bool 

          -- | Level-1 names that need to be rewritten to avoid capture.
        , subConflict1  :: Set n

          -- | Level-0 names that need to be rewritten to avoid capture.
        , subConflict0  :: Set n 

          -- | Rewriting stack for level-1 names.
        , subStack1     :: BindStack n

          -- | Rewriting stack for level-0 names.
        , subStack0     :: BindStack n  }


-- | Stack of anonymous binders that we've entered under during substitution. 
data BindStack n
        = BindStack
        { -- | Holds anonymous binders that were already in the program,
          --   as well as named binders that are being rewritten to anonymous ones.
          --   In the resulting expression all these binders will be anonymous.
          stackBinds    :: [Bind n]

          -- | Holds all binders, independent of whether they are being rewritten or not.
        , stackAll      :: [Bind n] 

          -- | Number of `BAnon` in `stackBinds`.
        , stackAnons    :: Int

          -- | Number of `BName` in `stackBinds`.
        , stackNamed    :: Int }


-- | Push a bind onto a bind stack, 
--   anonymising it if need be to avoid variable capture.
pushBind
        :: Ord n
        => Set n                  -- ^ Names that need to be rewritten.
        -> BindStack n            -- ^ Current bind stack.
        -> Bind n                 -- ^ Bind to push.
        -> (BindStack n, Bind n)  -- ^ New stack and possibly anonymised bind.

pushBind fns bs@(BindStack stack env dAnon dName) bb
 = case bb of
        -- Push already anonymous bind on stack.
        BAnon t                 
         -> ( BindStack (BAnon t   : stack) (BAnon t : env) (dAnon + 1) dName
            , BAnon t)
            
        -- If the binder needs to be rewritten then push the original name on the
        -- 'stackBinds' to remember this.
        BName n t
         | Set.member n fns     
         -> ( BindStack (BName n t : stack) (BAnon t : env)  dAnon       (dName + 1)
            , BAnon t)

         | otherwise
         -> ( BindStack stack               (BName n t : env) dAnon dName
            , bb)

        -- Binder was a wildcard.
        _ -> (bs, bb)


-- | Push several binds onto the bind stack,
--   anonymysing them if need be to avoid variable capture.
pushBinds :: Ord n => Set n -> BindStack n -> [Bind n]  -> (BindStack n, [Bind n])
pushBinds fns stack bs
        = mapAccumL (pushBind fns) stack bs


addBind :: Bind n -> BindStack n -> BindStack n
addBind b (BindStack stack env dAnon dName)
        = BindStack stack (b : env) dAnon dName



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

substBound (BindStack binds _ dAnon dName) u u'
        -- Bound name matches the one that we're substituting for.
        | UName n1 _   <- u
        , UName n2 _   <- u'
        , n1 == n2
        = Right (dAnon + dName)

        -- Bound index matches the one that we're substituting for.
        | UIx  i1 _     <- u
        , UIx  i2 _     <- u'
        , i1 + dAnon == i2 
        = Right (dAnon + dName)

        -- The Bind for this name was rewritten to avoid variable capture,
        -- so we also have to update the bound occurrence.
        | UName _ t     <- u'
        , Just ix       <- findIndex (boundMatchesBind u') binds
        = Left $ UIx ix t

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


-------------------------------------------------------------------------------
-- | Push a level-1 binder on the rewrite stack.
bindT :: Ord n => Sub n -> Bind n -> (Sub n, Bind n)
bindT sub b 
 = let  (stackT', b')     = pushBind (subConflict1 sub) (subStack1 sub) b
   in   (sub { subStack1  = stackT' }, b')


-- | Push a level-0 binder on the rewrite stack.
bindX :: Ord n => Sub n -> Bind n -> (Sub n, Bind n)
bindX sub b 
 = let  b1                  = rewriteWith sub b
        (stackX', b2)       = pushBind (subConflict0 sub) (subStack0 sub) b1
   in   ( sub { subStack0   = stackX'
              , subShadow0  = namedBoundMatchesBind (subBound sub) b2}
        , b2)


-- | Push some level-0 binders on the rewrite stack.
bindXs :: Ord n => Sub n -> [Bind n] -> (Sub n, [Bind n])
bindXs = mapAccumL bindX


-- | Rewrite a level-1 binder to anonymous form if need be.
use1 :: Ord n => Sub n -> Bound n -> Bound n
use1 sub u
        | UName _ t             <- u
        , BindStack binds _ _ _ <- subStack1 sub
        , Just ix               <- findIndex (boundMatchesBind u) binds
        = UIx ix t

        | otherwise
        = u


-- | Rewrite a level-0 binder to anonymous form if need be.
use0 :: Ord n => Sub n -> Bound n -> Bound n
use0 sub u
        | UName _ t             <- u
        , BindStack binds _ _ _ <- subStack0 sub
        , Just ix               <- findIndex (boundMatchesBind u) binds
        = UIx ix (rewriteWith sub t)

        | otherwise
        = rewriteWith sub u


-------------------------------------------------------------------------------
class Rewrite (c :: * -> *) where
 -- | Rewrite names in some thing to anonymous form if need be.
 rewriteWith :: Ord n => Sub n -> c n -> c n 


instance Rewrite Bind where
 rewriteWith sub bb
  = replaceTypeOfBind  (rewriteWith sub (typeOfBind bb))  bb


instance Rewrite Bound where
 rewriteWith sub uu
  = replaceTypeOfBound (rewriteWith sub (typeOfBound uu)) uu


instance Rewrite LetMode where
 rewriteWith sub lm
  = case lm of
        LetStrict        -> lm
        LetLazy (Just t) -> LetLazy (Just $ rewriteWith sub t) 
        LetLazy Nothing  -> LetLazy Nothing


instance Rewrite Cast where
 rewriteWith sub cc
  = let down    = rewriteWith sub 
    in case cc of
        CastWeakenEffect  eff   -> CastWeakenEffect  (down eff)
        CastWeakenClosure clo   -> CastWeakenClosure (down clo)
        CastPurify w            -> CastPurify (down w)
        CastForget w            -> CastForget (down w)


instance Rewrite Type where
 rewriteWith sub tt 
  = let down    = rewriteWith 
    in case tt of
        TVar u          -> TVar (use1 sub u)
        TCon{}          -> tt

        TForall b t
         -> let (sub1, b')      = bindT sub b
                t'              = down  sub1 t
            in  TForall b' t'

        TApp t1 t2      -> TApp (down sub t1) (down sub t2)
        TSum ts         -> TSum (down sub ts)


instance Rewrite TypeSum where
 rewriteWith sub ts
        = Sum.fromList (Sum.kindOfSum ts)
        $ map (rewriteWith sub)
        $ Sum.toList ts


instance Rewrite Witness where
 rewriteWith sub ww
  = let down    = rewriteWith 
    in case ww of
        WVar u          -> WVar  (use0 sub u)
        WCon{}          -> ww
        WApp  w1 w2     -> WApp  (down sub w1) (down sub w2)
        WJoin w1 w2     -> WJoin (down sub w1) (down sub w2)
        WType t         -> WType (down sub t)

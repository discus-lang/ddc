
-- | Rewriting of anonymous binders to named binders.
module DDC.Core.Transform.Namify
        ( Namify        (..)
        , Namifier      (..)
        , makeNamifier
        , namifyUnique)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.Collect
import DDC.Type.Compounds
import Control.Monad
import DDC.Type.Env             (Env)
import qualified DDC.Type.Sum   as Sum
import qualified DDC.Type.Env   as Env
import Control.Monad.State.Strict


-- | Holds a function to rename binders, 
--   and the state of the renamer as we decend into the tree.
data Namifier s n
        = Namifier
        { -- | Create a new name for this bind that is not in the given
          --   environment.
          namifierNew   :: Env n -> Bind n -> State s n

          -- | Holds the current environment during namification.
        , namifierEnv   :: Env n

          -- | Stack of debruijn binders that have been rewritten during
          --   namification.
        , namifierStack :: [Bind n] }


-- | Construct a new namifier.
makeNamifier 
        :: (Env n -> Bind n -> State s n)       
                        -- ^ Function to rename binders.
                        --   The name chosen cannot be a member of the given
                        ---  environment.
        -> Env n        -- ^ Starting environment of names we cannot use.
        -> Namifier s n

makeNamifier new env
        = Namifier new env []


-- | Namify a thing, 
--   not reusing names already in the program.
namifyUnique
        :: (Ord n, Show n, Namify c, BindStruct c)
        => (Env n -> Namifier s n)
        -> (Env n -> Namifier s n)
        -> c n
        -> State s (c n)

namifyUnique mkNamK mkNamT xx
 = let  (tbinds, xbinds) = collectBinds xx
        namK    = mkNamK (Env.fromList tbinds)
        namT    = mkNamT (Env.fromList xbinds)
   in   namify namK namT xx


-- Namify ---------------------------------------------------------------------
class Namify (c :: * -> *) where
 -- | Rewrite anonymous binders to named binders in a thing.
 namify :: (Ord n, Show n)
        => Namifier s n         -- ^ Namifier for type names (level-1)
        -> Namifier s n         -- ^ Namifier for exp names (level-0)
        -> c n                  -- ^ Rewrite binders in this thing.
        -> State s (c n)


instance Namify Type where
 namify tnam xnam tt
  = let down = namify tnam xnam
    in case tt of
        TVar u          -> liftM TVar (rewriteT tnam u)     

        TCon{}          
         ->     return tt

        TForall b t
         -> do  (tnam', b')     <- pushT tnam b
                t'              <- namify tnam' xnam t
                return  $ TForall b' t'

        TApp t1 t2      -> liftM2 TApp (down t1) (down t2)
        TSum ts         
         -> do  ts'     <- mapM down $ Sum.toList ts
                return  $ TSum $ Sum.fromList (Sum.kindOfSum ts) ts'


instance Namify (Module a) where
 namify tnam xnam mm 
  = do  body'    <- namify tnam xnam $ moduleBody mm
        return  $ mm { moduleBody = body' }


instance Namify LetMode where
 namify tnam xnam mm
  = case mm of
        LetStrict               -> return mm
        LetLazy Nothing         -> return mm
        LetLazy (Just w)        -> liftM (LetLazy . Just) $ namify tnam xnam w


instance Namify Witness where
 namify tnam xnam ww
  = let down = namify tnam xnam
    in case ww of
        WVar u          -> liftM  WVar  (rewriteX tnam xnam u)
        WCon{}          -> return ww
        WApp  w1 w2     -> liftM2 WApp  (down w1) (down w2)
        WJoin w1 w2     -> liftM2 WJoin (down w1) (down w2)
        WType t         -> liftM  WType (down t)


instance Namify (Exp a) where
 namify tnam xnam xx
  = let down = namify tnam xnam
    in case xx of
        XVar a u        -> liftM2 XVar (return a) (rewriteX tnam xnam u)
        XCon{}          -> return xx

        XLAM a b x
         -> do  (tnam', b')     <- pushT  tnam b
                x'              <- namify tnam' xnam x
                return $ XLAM a b' x'

        XLam a b x
         -> do  (xnam', b')     <- pushX  tnam xnam b
                x'              <- namify tnam xnam' x
                return $ XLam a b' x'

        XApp  a x1 x2   
         -> liftM3 XApp     (return a) (down x1)  (down x2)

        XLet  a (LLet mode b x1) x2
         -> do  mode'           <- down mode
                x1'             <- namify tnam xnam x1
                (xnam', b')     <- pushX  tnam xnam b
                x2'             <- namify tnam xnam' x2
                return $ XLet a (LLet mode' b' x1') x2'

        XLet a (LRec bxs) x2
         -> do  let (bs, xs)    = unzip bxs
                (xnam', bs')    <- pushXs tnam xnam bs
                xs'             <- mapM (namify tnam xnam') xs
                x2'             <- namify tnam xnam' x2
                return $ XLet a (LRec (zip bs' xs')) x2'

        XLet a (LLetRegion b bs) x2
         -> do  (tnam', b')     <- pushT tnam b
                (xnam', bs')    <- pushXs tnam' xnam bs
                x2'             <- namify tnam' xnam' x2
                return $ XLet a (LLetRegion b' bs') x2'

        XLet a (LWithRegion u) x2
         -> do  u'              <- rewriteX tnam xnam u
                x2'             <- down x2
                return  $ XLet a (LWithRegion u') x2'

        XCase a x1 alts -> liftM3 XCase    (return a) (down x1)  (mapM down alts)
        XCast a c  x    -> liftM3 XCast    (return a) (down c)   (down x)
        XType t         -> liftM  XType    (down t)
        XWitness w      -> liftM  XWitness (down w)


instance Namify (Alt a) where
 namify tnam xnam (AAlt PDefault x)
  = liftM (AAlt PDefault) (namify tnam xnam x)

 namify tnam xnam (AAlt (PData u bs) x)
  = do  (xnam', bs')    <- pushXs tnam xnam bs
        x'              <- namify tnam xnam' x
        return  $ AAlt (PData u bs') x'


instance Namify Cast where
 namify tnam xnam cc
  = let down = namify tnam xnam
    in case cc of
        CastWeakenEffect  eff   -> liftM CastWeakenEffect  (down eff)
        CastWeakenClosure clo   -> liftM CastWeakenClosure (down clo)
        CastPurify w            -> liftM CastPurify (down w)
        CastForget w            -> liftM CastForget (down w)


-- | Rewrite level-1 anonymous binders.
rewriteT :: Show n
        => Namifier s n
        -> Bound n
        -> State s (Bound n)

rewriteT tnam u
 = case u of
        UIx i t
         -> case lookup i (zip [0..] (namifierStack tnam)) of
                Just (BName n _) -> return $ UName n t
                _                -> return u

        _       -> return u


-- | Rewrite level-0 anonymous binders.
rewriteX :: (Show n, Ord n)
        => Namifier s n
        -> Namifier s n
        -> Bound n
        -> State s (Bound n)

rewriteX tnam xnam u
 = case u of
        UIx i t
         -> case lookup i (zip [0..] (namifierStack xnam)) of
                Just (BName n _) 
                 -> do  t'      <- namify tnam xnam t
                        return  $ UName n t'

                _                -> return u

        _       -> return u


-- Push -----------------------------------------------------------------------
-- Chosing new names for anonymous binders and pushing them on the stack.

-- | Push a level-0 binder on the stack.
--   When we do this we also rewrite any indices in its type annotation.
pushX   :: (Ord n, Show n)
        => Namifier s n
        -> Namifier s n
        -> Bind n
        -> State s (Namifier s n, Bind n) 

pushX tnam xnam b
 = do   t'      <- namify tnam xnam (typeOfBind b)
        let b'  = replaceTypeOfBind t' b
        push xnam b'


-- | Push some level-0 binders on the stack.
--   When we do this we also rewrite their type annotations.
pushXs   :: (Ord n, Show n)
        => Namifier s n
        -> Namifier s n
        -> [Bind n]
        -> State s (Namifier s n, [Bind n])

pushXs _tnam xnam []    
        = return (xnam, [])

pushXs tnam xnam (b:bs)
 = do   (xnam1, b')      <- pushX  tnam xnam  b
        (xnam2, bs')     <- pushXs tnam xnam1 bs
        return (xnam2, b' : bs')


-- | Push a level-1 binder on the stack.
pushT   :: Ord n
        => Namifier s n
        -> Bind n
        -> State s (Namifier s n, Bind n)
pushT   = push


-- | Rewrite an anonymous binder and push it on the stack.
push    :: Ord n 
        => Namifier s n 
        -> Bind n 
        -> State s (Namifier s n, Bind n)

push nam b
 = case b of
        BAnon t
         -> do  n       <- namifierNew nam (namifierEnv nam) b
                let b'  = BName n t
                return  ( nam   { namifierStack = b' : namifierStack nam 
                                , namifierEnv   = Env.extend b (namifierEnv nam) }
                        , b' )
        _ ->    return  ( nam   { namifierEnv   = Env.extend b (namifierEnv nam) }
                        , b)

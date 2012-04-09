module DDC.Core.Transform.Namify
        ( Namifier      (..)
        , makeNamifier
        , namify)
where
import DDC.Core.Module
import DDC.Core.Exp
import Control.Monad
import DDC.Type.Env             (Env)
import qualified DDC.Type.Sum   as Sum
import qualified DDC.Type.Env   as Env
import Control.Monad.State.Strict


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

makeNamifier 
        :: (Env n -> Bind n -> State s n)
        -> Env n
        -> Namifier s n

makeNamifier new env
        = Namifier new env []


-- Namify ---------------------------------------------------------------------
class Namify (c :: * -> *) where
 namify :: Ord n
        => Namifier s n
        -> Namifier s n
        -> c n -> State s (c n)


instance Namify Type where
 namify tnam xnam tt
  = let down = namify tnam xnam
    in case tt of
        TVar (UIx i k)
         -> do  let (BName n _) = namifierStack tnam !! i
                return  $ TVar (UName n k)

        TVar{}          -> return tt
        TCon{}          -> return tt

        TForall b t
         -> do  (tnam', b')     <- push tnam b
                t'              <- namify tnam' xnam t
                return  $ TForall b' t'

        TApp t1 t2      -> liftM2 TApp (down t1) (down t2)
        TSum ts         
         -> do  ts'     <- mapM down $ Sum.toList ts
                return  $ TSum $ Sum.fromList (Sum.kindOfSum ts) ts'


instance Namify (Module a) where
 namify tnam xnam mm 
  = do  lts'    <- mapM (namify tnam xnam) $ moduleLets mm
        return  $ mm { moduleLets = lts' }


instance Namify (Lets a) where
 namify tnam xnam ll
  = let down = namify tnam xnam
    in case ll of
        LLet mode b x
         -> do  mode'           <- down mode           
                (xnam', b')     <- push xnam b
                x'              <- namify tnam xnam' x
                return          $ LLet mode' b' x'

        _       -> error "namify finish me"


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
        WVar (UIx i t)
         -> do  let (BName n _) = namifierStack xnam !! i
                return  $ WVar (UName n t)

        WVar{}          -> return ww
        WCon{}          -> return ww
        WApp  w1 w2     -> liftM2 WApp  (down w1) (down w2)
        WJoin w1 w2     -> liftM2 WJoin (down w1) (down w2)
        WType t         -> liftM  WType (down t)


instance Namify (Exp a) where
 namify tnam xnam xx
  = let down = namify tnam xnam
    in case xx of
        XVar a (UIx i t)
         -> do  let (BName n _)  = namifierStack xnam !! i
                return  $ XVar a (UName n t)

        XVar{}          -> return xx
        XCon{}          -> return xx

        XLAM a b x
         -> do  (tnam', b')     <- push tnam b
                x'              <- namify tnam' xnam x
                return $ XLAM a b' x'

        XLam a b x
         -> do  (xnam', b')     <- push xnam b
                x'              <- namify tnam xnam' x
                return $ XLam a b' x'

        XApp a x1 x2    -> liftM3 XApp  (return a) (down x1) (down x2)
        XCase a x1 alts -> liftM3 XCase (return a) (down x1) (mapM down alts)

        _ -> error "finish me"


instance Namify (Alt a) where
 namify 
  = error "finish me"


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


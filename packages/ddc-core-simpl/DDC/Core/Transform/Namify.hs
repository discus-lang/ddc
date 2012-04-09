module DDC.Core.Transform.Namify
        ( Namifier      (..)
        , makeNamifier
        , namify)
where
import DDC.Core.Exp
import Control.Monad
import DDC.Type.Env             (Env)
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


instance Namify (Exp a) where
 namify knam tnam xx
  = let 
        push nam b@(BAnon t)
         = do   n       <- namifierNew nam (namifierEnv nam) b
                let b'  = BName n t
                return  ( nam   { namifierStack = b' : namifierStack nam 
                                , namifierEnv   = Env.extend b (namifierEnv nam) }
                        , b' )
        push nam b
         =      return  ( nam   { namifierEnv   = Env.extend b (namifierEnv nam) }
                        , b)

        down = namify knam tnam

    in case xx of
        XVar a (UIx i t)
         -> do  let (BName n _)  = namifierStack tnam !! i
                return  $ XVar a (UName n t)

        XVar{}
         ->     return xx

        XCon{} 
         ->     return xx

        XLAM a b x
         -> do  (knam', b')     <- push knam b
                x'              <- namify knam' tnam x
                return $ XLAM a b' x'

        XLam a b x
         -> do  (tnam', b')     <- push tnam b
                x'              <- namify knam tnam' x
                return $ XLam a b' x'

        XApp a x1 x2
         -> liftM3 XApp  (return a) (down x1) (down x2)

        XCase a x1 alts
         -> liftM3 XCase (return a) (down x1) (mapM down alts)

        _ -> error "finish me"


instance Namify (Alt a) where
 namify 
  = error "finish me"



module DDC.Core.Transform.TransformX
        ( TransformUpMX(..)
        , transformUpX)
where
import DDC.Core.Exp
import DDC.Type.Env             (Env)
import Data.Functor.Identity
import Control.Monad
import qualified DDC.Type.Env   as Env


class TransformUpMX m (c :: * -> * -> *) where
 -- | Bottom-up monadic rewrite of all core expressions in a thing.
 transformUpMX
        :: Ord n
        => (Env n -> Env n -> Exp a n -> m (Exp a n))
        ->  Env n -> Env n -> c a n   -> m (c a n)

instance Monad m => TransformUpMX m Exp where
 transformUpMX f kenv tenv xx
  = (f kenv tenv =<<)
  $ case xx of
        XVar{}          -> return xx
        XCon{}          -> return xx

        XApp a x1 x2    
         -> liftM3 XApp (return a) 
                        (transformUpMX f kenv tenv x1) 
                        (transformUpMX f kenv tenv x2)

        XLAM a b x1
         -> liftM3 XLAM (return a) (return b)
                        (transformUpMX f (Env.extend b kenv) tenv x1)

        XLam a b  x1    
         -> liftM3 XLam (return a) (return b) 
                        (transformUpMX f kenv (Env.extend b tenv) x1)

        XType _         -> return xx

        _               -> error "transformUpX: not finished"


-- | Bottom up rewrite of all core expressions in a thing.
transformUpX
        :: forall (c :: * -> * -> *) a n
        .  (Ord n, TransformUpMX Identity c)
        => (Env n -> Env n -> Exp a n -> Exp a n) 
        ->  Env n -> Env n -> c a n   -> c a n

transformUpX f kenv tenv xx
        = runIdentity 
        $ transformUpMX 
                (\kenv' tenv' x -> return (f kenv' tenv' x)) 
                kenv tenv xx

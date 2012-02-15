
-- | General purpose tree walking boilerplate.
module DDC.Core.Transform.TransformX
        ( TransformUpMX(..)
        , transformUpX)
where
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Type.Env             (Env)
import Data.Functor.Identity
import Control.Monad
import qualified DDC.Type.Env   as Env


-- | Bottom up rewrite of all core expressions in a thing.
transformUpX
        :: forall (c :: * -> * -> *) a n
        .  (Ord n, TransformUpMX Identity c)
        => (Env n -> Env n -> Exp a n -> Exp a n)       
                        -- ^ The worker function is given the current kind and type environments.
        -> Env n        -- ^ Initial kind environment.
        -> Env n        -- ^ Initial type environment.
        -> c a n        -- ^ Transform this thing.
        -> c a n

transformUpX f kenv tenv xx
        = runIdentity 
        $ transformUpMX 
                (\kenv' tenv' x -> return (f kenv' tenv' x)) 
                kenv tenv xx


class TransformUpMX m (c :: * -> * -> *) where
 -- | Bottom-up monadic rewrite of all core expressions in a thing.
 transformUpMX
        :: Ord n
        => (Env n -> Env n -> Exp a n -> m (Exp a n))
                        -- ^ The worker function is given the current kind and type environments.
        -> Env n        -- ^ Initial kind environment.
        -> Env n        -- ^ Initial type environment.
        -> c a n        -- ^ Transform this thing.
        -> m (c a n)

instance Monad m => TransformUpMX m Exp where
 transformUpMX f kenv tenv xx
  = (f kenv tenv =<<)
  $ case xx of
        XVar{}          -> return xx
        XCon{}          -> return xx

        XLAM a b x1
         -> liftM3 XLAM (return a) (return b)
                        (transformUpMX f (Env.extend b kenv) tenv x1)

        XLam a b  x1    
         -> liftM3 XLam (return a) (return b) 
                        (transformUpMX f kenv (Env.extend b tenv) x1)

        XApp a x1 x2    
         -> liftM3 XApp (return a) 
                        (transformUpMX f kenv tenv x1) 
                        (transformUpMX f kenv tenv x2)

        XLet a lts x
         -> do  lts'      <- transformUpMX f kenv tenv lts
                let kenv' = Env.extends (specBindsOfLets lts')   kenv
                let tenv' = Env.extends (valwitBindsOfLets lts') tenv
                x'        <- transformUpMX f kenv' tenv' x
                return  $ XLet a lts' x'
                
        XCase a x alts
         -> liftM3 XCase (return a)
                         (transformUpMX f kenv tenv x)
                         (mapM (transformUpMX f kenv tenv) alts)

        XCast a c x       
         -> liftM3 XCast
                        (return a) (return c)
                        (transformUpMX f kenv tenv x)

        XType _         -> return xx
        XWitness _      -> return xx


instance Monad m => TransformUpMX m Lets where
 transformUpMX f kenv tenv xx
  = case xx of
        LLet m b x
         -> liftM3 LLet (return m) (return b)
                        (transformUpMX f kenv tenv x)
        
        LRec bxs
         -> do  let (bs, xs) = unzip bxs
                let tenv'    = Env.extends bs tenv
                xs'          <- mapM (transformUpMX f kenv tenv') xs
                return       $ LRec $ zip bs xs'

        LLetRegion{}    -> return xx
        LWithRegion{}    -> return xx


instance Monad m => TransformUpMX m Alt where
 transformUpMX f kenv tenv alt
  = case alt of
        AAlt p@(PData _ bsArg) x
         -> let tenv'   = Env.extends bsArg tenv
            in  liftM2  AAlt (return p) 
                        (transformUpMX f kenv tenv' x)

        AAlt PDefault x
         ->     liftM2  AAlt (return PDefault)
                        (transformUpMX f kenv tenv x) 

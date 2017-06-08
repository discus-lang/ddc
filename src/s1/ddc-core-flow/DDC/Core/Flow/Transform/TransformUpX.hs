
-- | General purpose tree walking boilerplate.
module DDC.Core.Flow.Transform.TransformUpX
        ( TransformUpMX(..)
        , transformUpX
        , transformUpX'

          -- * Via the Simple AST
        , transformSimpleUpMX
        , transformSimpleUpX
        , transformSimpleUpX')
where
import DDC.Core.Exp.Annot
import DDC.Core.Transform.TransformUpX  
import DDC.Core.Flow.Transform.Annotate
import DDC.Core.Flow.Transform.Deannotate
import Data.Functor.Identity
import DDC.Core.Env.EnvX                        (EnvX)
import qualified DDC.Core.Flow.Exp.Simple.Exp   as S
import qualified DDC.Core.Env.EnvX              as EnvX


-- Simple ---------------------------------------------------------------------
-- | Like `transformUpMX`, but the worker takes the Simple version of the AST.
--
--   * To avoid repeated conversions between the different versions of the AST,
--     the worker should return `Nothing` if the provided expression is unchanged.
transformSimpleUpMX 
        :: (Ord n, TransformUpMX m c, Monad m)
        => (EnvX n -> S.Exp a n -> m (Maybe (S.Exp a n)))
                        -- ^ The worker function is given the current
                        --     kind and type environments.
        -> EnvX n       -- ^ Initial type environment.
        -> c a n        -- ^ Transform thing thing.
        -> m (c a n)

transformSimpleUpMX f env0 xx0
 = let  
        f' env xx
         = do   let a    = annotOfExp xx
                let sxx  = deannotate (const Nothing) xx
                msxx'    <- f env sxx
                case msxx' of
                     Nothing   -> return $ xx
                     Just sxx' -> return $ annotate a sxx'

   in   transformUpMX f' env0 xx0


-- | Like `transformUpX`, but the worker takes the Simple version of the AST.
--
--   * To avoid repeated conversions between the different versions of the AST,
--     the worker should return `Nothing` if the provided expression is unchanged.
transformSimpleUpX
        :: forall (c :: * -> * -> *) a n
        .  (Ord n, TransformUpMX Identity c)
        => (EnvX n -> S.Exp a n -> Maybe (S.Exp a n))
                        -- ^ The worker function is given the current
                        --     kind and type environments.
        -> EnvX n       -- ^ Initial type environment.
        -> c a n        -- ^ Transform this thing.
        -> c a n

transformSimpleUpX f env xx
        = runIdentity 
        $ transformSimpleUpMX 
                (\env' x -> return (f env' x)) env xx


-- | Like `transformUpX'`, but the worker takes the Simple version of the AST.
--
--   * To avoid repeated conversions between the different versions of the AST,
--     the worker should return `Nothing` if the provided expression is unchanged.
transformSimpleUpX'
        :: forall (c :: * -> * -> *) a n
        .  (Ord n, TransformUpMX Identity c)
        => (S.Exp a n -> Maybe (S.Exp a n))
                        -- ^ The worker function is given the current
                        --      kind and type environments.
        -> c a n        -- ^ Transform this thing.
        -> c a n

transformSimpleUpX' f xx
        = transformSimpleUpX (\_ -> f) EnvX.empty xx


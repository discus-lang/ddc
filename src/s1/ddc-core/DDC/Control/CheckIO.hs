
-- | A simple exception monad where the inner computation is in IO.
module DDC.Control.CheckIO
        ( CheckM (..), MonadIO(..)
        , throw
        , runCheck
        , evalCheck
        , mapErr
        , get
        , put)
where
import Control.Monad
import Control.Monad.IO.Class


-- | Checker monad maintains some state and manages errors during type checking.
data CheckM s err a
        = CheckM (s -> IO (s, Either err a))


instance Functor (CheckM s err) where
 fmap   = liftM


instance Applicative (CheckM s err) where
 (<*>)  = ap
 pure   = return


instance Monad (CheckM s err) where
 return !x
  = CheckM (\s -> return (s, Right x))
 {-# INLINE return #-}

 (>>=) !(CheckM f) !g
  = CheckM $ \s
  -> f s >>= \r -> case r of
        (s', Left err)  -> return (s', Left err)
        (s', Right x)   -> s `seq` x `seq` runCheck s' (g x)
 {-# INLINE (>>=) #-}


instance MonadIO (CheckM s err) where
 liftIO action
  = CheckM $ \s -> action >>= \r -> return (s, Right r)


-- | Run a checker computation,
--      returning the result and new state.
runCheck :: s -> CheckM s err a -> IO (s, Either err a)
runCheck s (CheckM f)   = f s
{-# INLINE runCheck #-}


-- | Run a checker computation,
--      ignoreing the final state.
evalCheck :: s -> CheckM s err a -> IO (Either err a)
evalCheck s m   = fmap snd $ runCheck s m
{-# INLINE evalCheck #-}


-- | Throw a type error in the monad.
throw :: err -> CheckM s err a
throw !e        = CheckM $ \s -> return (s, Left e)
{-# INLINE throw #-}


-- | Map a function over any thrown errors.
mapErr :: (err1 -> err2) -> CheckM s err1 a -> CheckM s err2 a
mapErr fErr !(CheckM f)
 = CheckM $ \s
 -> f s >>= \r -> case r of
        (s', Left err)  -> return (s', Left (fErr err))
        (s', Right x)   -> return (s', Right x)


-- | Get the state from the monad.
get :: CheckM s err s
get =  CheckM $ \s -> return (s, Right s)
{-# INLINE get #-}


-- | Put a new state into the monad.
put :: s -> CheckM s err ()
put s
 =  CheckM $ \_ -> return (s, Right ())
{-# INLINE put #-}



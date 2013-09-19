
-- | A simple exception monad.
module DDC.Control.Monad.Check
        ( CheckM (..)
        , throw
        , runCheck
        , evalCheck
        , get
        , put)
where


-- | Checker monad maintains some state and manages errors during type checking.
data CheckM s err a
        = CheckM (s -> (s, Either err a))


instance Monad (CheckM s err) where
 return !x   
  = CheckM (\s -> (s, Right x))
 {-# INLINE return #-}

 (>>=) !(CheckM f) !g  
  = CheckM $ \s  
  -> case f s of
        (s', Left err)  -> (s', Left err)
        (s', Right x)   -> s `seq` x `seq` runCheck s' (g x)
 {-# INLINE (>>=) #-}


-- | Run a checker computation,
--      returning the result and new state.
runCheck :: s -> CheckM s err a -> (s, Either err a)
runCheck s (CheckM f)   = f s
{-# INLINE runCheck #-}


-- | Run a checker computation, 
--      ignoreing the final state.
evalCheck :: s -> CheckM s err a -> Either err a
evalCheck s m   = snd $ runCheck s m
{-# INLINE evalCheck #-}


-- | Throw a type error in the monad.
throw :: err -> CheckM s err a
throw !e        = CheckM $ \s -> (s, Left e)
{-# INLINE throw #-}


-- | Get the state from the monad.
get :: CheckM s err s
get =  CheckM $ \s -> (s, Right s)
{-# INLINE get #-}


-- | Put a new state into the monad.
put :: s -> CheckM s err ()
put s 
 =  CheckM $ \_ -> (s, Right ())
{-# INLINE put #-}


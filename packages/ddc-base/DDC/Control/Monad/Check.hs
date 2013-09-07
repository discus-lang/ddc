
-- | A simple exception monad.
module DDC.Control.Monad.Check
        ( CheckM (..)
        , throw
        , runCheck
        , evalCheck)
where


-- | Checker monad maintains some state and manages errors during type checking.
data CheckM s err a
        = CheckM (s -> (s, Either err a))


instance Monad (CheckM s err) where
 return !x   
  = CheckM (\s -> (s, Right x))
 {-# INLINE return #-}

 -- (>>=) :: CheckM s err a -> (a -> Check s err b) -> Check s err b
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


-- | Run a checker computation, 
--      ignoreing the final state.
evalCheck :: s -> CheckM s err a -> Either err a
evalCheck s m   = snd $ runCheck s m


-- | Throw a type error in the monad.
throw :: err -> CheckM s err a
throw !e        = CheckM $ \s -> (s, Left e)



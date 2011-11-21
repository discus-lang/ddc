
module DDC.Type.Check.Monad
        ( CheckM (..)
        , throw
        , result)
where

-- | Type checking monad.
data CheckM err a
        = CheckM (Either err a)

instance Monad (CheckM err) where
 return x   = CheckM (Right x)
 (>>=) m f  
  = case m of
          CheckM (Left err)     -> CheckM (Left err)
          CheckM (Right x)      -> f x

          
-- | Throw a type error in the monad.
throw :: err -> CheckM err a
throw e       = CheckM $ Left e


-- | Take the result from a check monad.
result :: CheckM err a -> Either err a
result (CheckM r)       = r



module Text.Lexer.Inchworm.Scanner
        ( Scanner (..)
        , scanSourceToList)
where
import Text.Lexer.Inchworm.Source


-- | Scanner of input tokens. We pull some tokens from the source
--   and maybe return a value.
data Scanner m is a
        = Scanner
        { runScanner  :: Source m is -> m (Maybe a) }


instance Monad m => Functor (Scanner m is) where
 fmap f (Scanner load)
  =  Scanner $ \source 
  -> do r       <- load source
        case r of
         Nothing        -> return Nothing
         Just x         -> return $ Just $ f x


scanSourceToList
        :: Monad m
        => Source m [i] -> Scanner m [i] a -> m [a]

scanSourceToList ss (Scanner load)
 = go []
 where  go acc
         =  load ss >>= \result
         -> case result of
                Just x  -> go (x : acc)
                Nothing -> return $ reverse acc



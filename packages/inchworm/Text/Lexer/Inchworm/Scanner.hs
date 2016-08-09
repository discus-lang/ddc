
module Text.Lexer.Inchworm.Scanner
        ( Scanner (..)
        , scanSourceToList)
where
import Text.Lexer.Inchworm.Source


-- | Scanner of input tokens. We pull some tokens from the source
--   and maybe return a value.
data Scanner m loc input a
        = Scanner
        { runScanner  :: Source m loc input -> m (Maybe a) }


instance Monad m => Functor (Scanner m loc input) where
 fmap f (Scanner load)
  =  Scanner $ \source 
  -> do r       <- load source
        case r of
         Nothing        -> return Nothing
         Just x         -> return $ Just $ f x
 {-# INLINE fmap #-}

-- | Apply a scanner to a source of input tokens,
--   where the tokens are represented as a lazy list.
--
--   The result values are also produced in a lazy list.
--
scanSourceToList
        :: Monad  m
        => Source m loc [i] -> Scanner m loc [i] a -> m [a]

scanSourceToList ss (Scanner load)
 = go []
 where  go acc
         =  load ss >>= \result
         -> case result of
                Just x  -> go (x : acc)
                Nothing -> return $ reverse acc

{-# SPECIALIZE INLINE
     scanSourceToList
      :: Source  IO Loc [Char]
      -> Scanner IO Loc [Char] a
      -> IO [a] 
  #-}

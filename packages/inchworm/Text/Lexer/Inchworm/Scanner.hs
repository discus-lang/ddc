{-# LANGUAGE InstanceSigs #-}

module Text.Lexer.Inchworm.Scanner
        ( Scanner (..)
        , scanSourceToList)
where
import Text.Lexer.Inchworm.Source


-- | Scanner of input tokens that produces a result value
--   of type @a@ when successful.
data Scanner m loc input a
        = Scanner
        { runScanner  :: Source m loc input -> m (Maybe a) }


instance Monad m
      => Functor (Scanner m loc input) where
 fmap f (Scanner load)
  =  Scanner $ \source 
  -> do r       <- load source
        case r of
         Nothing        -> return Nothing
         Just x         -> return $ Just $ f x
 {-# INLINE fmap #-}


instance Monad m
      => Applicative (Scanner m loc input) where
 pure x 
  = Scanner $ \_ -> return (Just x)

 (<*>) (Scanner loadF) (Scanner loadX)
  =  Scanner $ \ss
  -> do mf      <- loadF ss
        case mf of
         Nothing
          -> return Nothing

         Just f
          -> do mx      <- loadX ss
                case mx of
                 Nothing        -> return Nothing
                 Just x         -> return $ Just (f x)

instance Monad m
      => Monad (Scanner m loc input) where
 return x
  = Scanner $ \_ -> return (Just x)

 (>>=) (Scanner loadX) f
  =  Scanner $ \ss
  -> do mx        <- loadX ss
        case mx of
         Nothing  -> return Nothing
         Just x   -> runScanner (f x) ss


-- | Apply a scanner to a source of input tokens,
--   where the tokens are represented as a lazy list.
--
--   The result values are also produced in a lazy list.
--
scanSourceToList
        :: Monad  m
        => Source m loc [i] -> Scanner m loc [i] a 
        -> m ([a], loc, [i])

scanSourceToList ss (Scanner load)
 = go []
 where  go acc
         =  load ss >>= \result
         -> case result of
                Just x  -> go (x : acc)
                Nothing 
                 -> do  (loc, src') <- sourceRemaining ss
                        return (reverse acc, loc, src')

{-# SPECIALIZE INLINE
     scanSourceToList
      :: Source  IO Location [Char]
      -> Scanner IO Location [Char] a
      -> IO ([a], Location, [Char])
  #-}

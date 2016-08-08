{-# LANGUAGE BangPatterns, RankNTypes, TypeFamilies, FlexibleContexts #-}
module Text.Lexer.Inchworm.Source
        ( Source   (..), Elem (..)
        , Sequence (..)
        , makeListSourceIO)
where
import Data.IORef
import qualified Data.List              as List
import Prelude  hiding (length)


---------------------------------------------------------------------------------------------------
class Sequence is where
 type Elem is
 length :: is -> Int
 index  :: is -> Int -> Maybe (Elem is)


instance Sequence [a] where
 type Elem [a]  = a
 length         = List.length

 index ss0 ix0
  = go ss0 ix0
  where
        go []       _   = Nothing
        go (x : xs) 0   = Just x
        go (x : xs) n   = go xs (n - 1)



-- | Source of data values of type 'i'.
data Source m is
        = Source
        { -- | Skip over values from the source that match the given predicate.
          sourceSkip    :: (Elem is -> Bool) -> m ()

          -- | Try to evaluate the given computation that may pull values
          --   from the source. If it returns Nothing then rewind the 
          --   source to the original position.
        , sourceTry     :: forall a. m (Maybe a) -> m (Maybe a)

          -- | Pull a value from the source,
          --   provided it matches the given predicate.
        , sourcePull    :: (Elem is -> Bool) -> m (Maybe (Elem is))

          -- | Use a fold function to select a some consecutive tokens from the source
          --   that we want to process, also passing the current index to the fold function.
        , sourcePulls   :: forall s
                        .  Maybe Int 
                        -> (Int -> Elem is -> s -> Maybe s)
                        -> s
                        -> m (Maybe is)
        }


---------------------------------------------------------------------------------------------------
-- | Make a source from a list of values,
--   maintaining the state in the IO monad.
makeListSourceIO 
        :: Eq i => [i] -> IO (Source IO [i])

makeListSourceIO cs0
 =  newIORef cs0 >>= \ref
 -> return 
 $  Source 
        (skipListSourceIO  ref)
        (tryListSourceIO   ref)
        (pullListSourceIO  ref)
        (pullsListSourceIO ref)
 where
        -- Skip values from the source.
        skipListSourceIO ref pred
         = do
                cc0     <- readIORef ref
                let eat !cc
                     = case cc of
                        []      
                         -> return ()

                        c : cs  
                         |  pred c
                         -> eat cs

                         | otherwise 
                         -> do  writeIORef ref (c : cs)
                                return ()

                eat cc0


        -- Try to run the given computation,
        -- reverting source state changes if it returns Nothing.
        tryListSourceIO ref comp 
         = do   cc      <- readIORef ref
                mx      <- comp
                case mx of
                 Just i  
                  -> return (Just i)

                 Nothing 
                  -> do writeIORef ref cc
                        return Nothing


        -- Pull a single value from the source.
        pullListSourceIO ref pred
         = do  cc      <- readIORef ref
               case cc of
                []
                 -> return Nothing

                c : cs 
                 |  pred c 
                 -> do writeIORef ref cs
                       return $ Just c

                 | otherwise
                 ->    return Nothing


        -- Pull a vector of values that match the given predicate
        -- from the source.
        pullsListSourceIO ref mLenMax work s0
         = do   cc0     <- readIORef ref

                let eat !ix !cc !acc !s
                     | Just mx  <- mLenMax
                     , ix >= mx
                     = return (ix, cc, reverse acc)

                     | otherwise
                     = case cc of
                        []      
                         -> return (ix, cc, reverse acc)

                        c : cs
                         -> case work ix c s of
                                Nothing -> return (ix, cc, reverse acc)
                                Just s' -> eat (ix + 1) cs (c : acc) s'

                (len, cc', acc) 
                 <- eat 0 cc0 [] s0

                case len of
                 0      -> return Nothing
                 _      -> do
                        writeIORef ref cc'
                        return  $ Just acc


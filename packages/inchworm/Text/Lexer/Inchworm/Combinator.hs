{-# LANGUAGE FlexibleContexts #-}
module Text.Lexer.Inchworm.Combinator
        ( satisfies
        , accept,       accepts
        , from,         froms
        , alt,          alts
        , munchPred,    munchFold
        , skip)
where
import Text.Lexer.Inchworm.Source
import Text.Lexer.Inchworm.Scanner
import qualified Data.List              as List
import Prelude hiding (length)


-------------------------------------------------------------------------------
-- | Pull an input token if it matches the given predicate.
satisfies
        :: Monad m 
        => (Elem is -> Bool) -> Scanner m is (Elem is)

satisfies pred 
 =  Scanner $ \ss 
 -> sourcePull ss pred


-------------------------------------------------------------------------------
-- | Pull the next token if it is equal to the given one,
--   returning the given result value.
accept  :: (Monad m, Eq (Elem is))
        => Elem is -> a -> Scanner m is a
accept i a
 = from (\i'   -> if i == i'
                        then Just a
                        else Nothing)


-- | Accept a fixed length sequence of tokens,
--   returning the given result value.
accepts  :: (Monad m, Sequence is, Eq is)
         => is -> a -> Scanner m is a
accepts is a
 = froms (Just (length is))
         (\is' -> if is == is'
                        then Just a
                        else Nothing)


-------------------------------------------------------------------------------
-- | Use the given function to check whether to accept the next token,
--   returning the result it produces.
from    :: Monad m
        => (Elem is -> Maybe a) -> Scanner m is a

from accept 
 =  Scanner $ \ss
 -> sourceTry ss
 $  do  mx       <- sourcePull ss (const True)
        case mx of
         Nothing -> return Nothing
         Just x  -> return $ accept x


-- | Use the given function to check whether to accept 
--   a fixed length sequence of tokens.
froms   :: Monad m
        => Maybe Int -> (is -> Maybe a) -> Scanner m is a

froms mLen accept
 =  Scanner $ \ss
 -> sourceTry ss
 $  do  mx      <- sourcePulls ss mLen (\i c _ -> Just ()) ()
        case mx of
         Nothing -> return Nothing
         Just xs -> return $ accept xs


-------------------------------------------------------------------------------
-- | Combine two scanners into a new one that 
--   tries the first before the second.
alt     :: Monad m 
        => Scanner m is a -> Scanner m is a -> Scanner m is a
alt (Scanner scan1) (Scanner scan2)
 =  Scanner $ \ss
 -> do  mx              <- sourceTry ss (scan1 ss)
        case mx of
         Nothing        -> scan2 ss
         Just r         -> return (Just r)


-- | Combine a list of scanners into  new one.
alts    :: Monad m
        => [Scanner m is a] -> Scanner m is a
alts [] 
 = Scanner $ \ss -> return Nothing

alts (Scanner scan1 : xs)
 = Scanner $ \ss
 -> do  mx              <- sourceTry ss (scan1 ss)
        case mx of
         Nothing        -> runScanner (alts xs) ss
         Just r         -> return (Just r)


-------------------------------------------------------------------------------
-- | A scanner that skips tokens that match the given predicate,
--   before applying the given scanner.
skip    :: Monad m
        => (Elem i -> Bool) -> Scanner m i a -> Scanner m i a
skip pred (Scanner scan1)
 =  Scanner $ \ss
 -> do  sourceSkip ss pred
        scan1 ss


-------------------------------------------------------------------------------
munchPred 
        :: Monad m
        => Maybe Int
        -> (Int -> Elem is -> Bool) 
        -> (is  -> Maybe a)
        -> Scanner m is a

munchPred mLenMax pred accept
 = munchFold 
        mLenMax 
        (\ix i s -> if pred ix i then Just s else Nothing)
        ()
        accept


munchFold   
        :: Monad m
        => Maybe Int
        -> (Int -> Elem is -> s -> Maybe s) -> s
        -> (is  -> Maybe a)
        -> Scanner m is a

munchFold mLenMax work s0 accept
 =  Scanner $ \ss
 -> sourceTry ss
 $  do  mr              <- sourcePulls ss mLenMax work s0
        case mr of
         Nothing        -> return Nothing
         Just xs
          -> case accept xs of
                Nothing -> return Nothing
                Just x  -> return $ Just x

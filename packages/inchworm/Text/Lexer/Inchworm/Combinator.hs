{-# LANGUAGE FlexibleContexts #-}
module Text.Lexer.Inchworm.Combinator
        ( satisfies,    skip
        , accept,       accepts
        , from,         froms
        , alt,          alts
        , munchPred,    munchFold)
where
import Text.Lexer.Inchworm.Source
import Text.Lexer.Inchworm.Scanner
import Prelude hiding (length)


-------------------------------------------------------------------------------
-- | Accept the next token if it matches the given predicate,
--   returning that token as the result.
satisfies
        :: Monad m 
        => (Elem is -> Bool) -> Scanner m is (Elem is)

satisfies fPred
 =  Scanner $ \ss 
 -> sourcePull ss fPred
{-# INLINE satisfies #-}


-------------------------------------------------------------------------------
-- | Skips tokens that match the given predicate,
--   before applying the given argument scanner.
skip    :: Monad m
        => (Elem i -> Bool) -> Scanner m i a -> Scanner m i a
skip fPred (Scanner scan1)
 =  Scanner $ \ss
 -> do  sourceSkip ss fPred
        scan1 ss
{-# INLINE skip #-}


-------------------------------------------------------------------------------
-- | Accept the next input token if it is equal to the given one,
--   and return a result of type @a@.
accept  :: (Monad m, Eq (Elem is))
        => Elem is -> a -> Scanner m is a
accept i a
 = from (\i'   -> if i == i'
                        then Just a
                        else Nothing)
{-# INLINE accept #-}


-- | Accept a fixed length sequence of tokens that match the 
--   given sequence, and return a result of type @a@.
accepts  :: (Monad m, Sequence is, Eq is)
         => is -> a -> Scanner m is a
accepts is a
 = froms (Just (length is))
         (\is' -> if is == is'
                        then Just a
                        else Nothing)
{-# INLINE accepts #-}


-------------------------------------------------------------------------------
-- | Use the given function to check whether to accept the next token,
--   returning the result it produces.
from    :: Monad m
        => (Elem is -> Maybe a) -> Scanner m is a

from fAccept 
 =  Scanner $ \ss
 -> sourceTry ss
 $  do  mx       <- sourcePull ss (const True)
        case mx of
         Nothing -> return Nothing
         Just x  -> return $ fAccept x
{-# INLINE from #-}


-- | Use the given function to check whether to accept 
--   a fixed length sequence of tokens,
--   returning the result it produces.
froms   :: Monad m
        => Maybe Int -> (is -> Maybe a) -> Scanner m is a

froms mLen fAccept
 =  Scanner $ \ss
 -> sourceTry ss
 $  do  mx      <- sourcePulls ss mLen (\_ _ _ -> Just ()) ()
        case mx of
         Nothing -> return Nothing
         Just xs -> return $ fAccept xs
{-# INLINE froms #-}


-------------------------------------------------------------------------------
-- | Combine two argument scanners into a result scanner,
--   where the first argument scanner is tried before the second.
alt     :: Monad m 
        => Scanner m is a -> Scanner m is a -> Scanner m is a
alt (Scanner scan1) (Scanner scan2)
 =  Scanner $ \ss
 -> do  mx              <- sourceTry ss (scan1 ss)
        case mx of
         Nothing        -> scan2 ss
         Just r         -> return (Just r)
{-# INLINE alt #-}


-- | Combine a list of argumenet scanners a result scanner,
--   where each argument scanner is tried in turn until we find
--   one that matches (or not).
alts    :: Monad m
        => [Scanner m is a] -> Scanner m is a
alts [] 
 = Scanner $ \_ -> return Nothing

alts (Scanner scan1 : xs)
 = Scanner $ \ss
 -> do  mx              <- sourceTry ss (scan1 ss)
        case mx of
         Nothing        -> runScanner (alts xs) ss
         Just r         -> return (Just r)
{-# INLINE alts #-}


-------------------------------------------------------------------------------
-- | Munch input tokens, using a predicate to select the prefix to consider.
--
--   Given @munch (Just n) match accept@, we select a contiguous sequence
--   of tokens up to length n using the predicate @match@, then pass
--   that sequence to @accept@ to make a token out of it.
--   If @match@ selects no tokens, or @accept@ returns `Nothing`
--   then the scanner fails and no tokens are consumed from the source.
--
--   For example, to scan natural numbers use:
--
-- @
-- scanNat :: Monad m => Scanner m [Char] Integer
-- scanNat = munchPred Nothing match accept
--         where match _ c = isDigit c
--               accept cs = Just (read cs)
-- @
--
--   To match Haskell style constructor names use:
--
-- @
-- scanCon :: Monad m => Scanner m [Char] String
-- scanCon = munchPred Nothing match accept
--         where  match 0 c = isUpper    c
--                match _ c = isAlphaNum c
--                accept cs = Just cs
-- @
--
-- If you want to detect built-in constructor names like @Int@ and @Float@
-- then you can do it in the @accept@ function and produce a different
-- result constructor for each one.
--
munchPred 
        :: Monad m
        => Maybe Int
                -- ^ Maximum number of tokens to consider,
                --   or `Nothing` for no maximum.
        -> (Int -> Elem is -> Bool)
                -- ^ Predicate to decide whether to consider the next
                --   input token, also passed the index of the token
                --   in the prefix.

        -> (is  -> Maybe a)
                -- ^ Take the prefix of input tokens and decide
                --   whether to produce a result value.

        -> Scanner m is a
                -- ^ Scan a prefix of tokens of type @is@, 
                --   and produce a result of type @a@ if it matches.

munchPred mLenMax fPred fAccept
 = munchFold 
        mLenMax 
        (\ix i s -> if fPred ix i then Just s else Nothing)
        ()
        fAccept
{-# INLINE munchPred #-}


-------------------------------------------------------------------------------
-- | Like `munchPred`, but we can use a fold function to select the 
--   prefix of tokens to consider. This is useful when lexing comments, 
--   and string literals where consecutive tokens can have special meaning 
--   (ie escaped quote characters).
--
--   See the source of @scanHaskellChar@ in the "Text.Lexer.Inchworm.Char",
--   module for an example of its usage.
--
munchFold   
        :: Monad m
        => Maybe Int
                -- ^ Maximum number of tokens to consider,
                --   or `Nothing` for no maximum.
        -> (Int -> Elem is -> s -> Maybe s) 
                -- ^ Fold function to decide whether to consider the next
                --   input token. The next token will be considered if
                --   the function produces a `Just` with its new state.
                --   We stop considering tokens the first time it returns
                --   `Nothing`.
        -> s    -- ^ Initial state for the fold.
        -> (is  -> Maybe a)
                -- ^ Take the prefix of input tokens and decide
                --   whether to produce a result value.
        -> Scanner m is a
                -- ^ Scan a prefix of tokens of type @is@,
                --   and produce a result of type @a@ if it matches.

munchFold mLenMax work s0 acceptC
 =  Scanner $ \ss
 -> sourceTry ss
 $  do  mr              <- sourcePulls ss mLenMax work s0
        case mr of
         Nothing        -> return Nothing
         Just xs
          -> case acceptC xs of
                Nothing -> return Nothing
                Just x  -> return $ Just x
{-# INLINE munchFold #-}


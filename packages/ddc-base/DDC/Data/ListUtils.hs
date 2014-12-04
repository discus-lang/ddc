
-- | Replacements for unhelpful Haskell list functions.
--   If the standard versions are passed an empty list then we don't
--   get a proper source location.
module DDC.Data.ListUtils
        ( takeHead
        , takeTail
        , takeInit
        , takeMaximum
        , index
        , findDuplicates
        , stripSuffix)
where
import Data.List
import qualified Data.Set as Set


-- | Take the head of a list, or `Nothing` if it's empty.
takeHead :: [a] -> Maybe a
takeHead xs
 = case xs of
        []      -> Nothing
        _       -> Just (head xs)


-- | Take the tail of a list, or `Nothing` if it's empty.
takeTail :: [a] -> Maybe [a]
takeTail xs
 = case xs of
        []      -> Nothing
        _       -> Just (tail xs)


-- | Take the init of a list, or `Nothing` if it's empty.
takeInit :: [a] -> Maybe [a]
takeInit xs
 = case xs of
        []      -> Nothing
        _       -> Just (init xs)


-- | Take the maximum of a list, or `Nothing` if it's empty.
takeMaximum :: Ord a => [a] -> Maybe a
takeMaximum xs
 = case xs of
        []      -> Nothing
        _       -> Just (maximum xs)


-- | Retrieve the element at the given index,
--   or `Nothing if it's not there.
index :: [a] -> Int -> Maybe a
index [] _              = Nothing
index (x : _)  0        = Just x
index (_ : xs) i        = index xs (i - 1)



-- | Fine the duplicate values in a list.
findDuplicates :: Ord n => [n] -> [n]
findDuplicates xx
 = go (Set.fromList xx) xx
 where  go _  []            = []
        go ss (x : xs)
         | Set.member x ss  =     go (Set.delete x ss) xs
         | otherwise        = x : go ss xs


-- | Drops the given suffix from a list.
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suff xx
 = case stripPrefix (reverse suff) (reverse xx) of
        Nothing -> Nothing
        Just xs -> Just $ reverse xs



-- | Replacements for unhelpful Haskell list functions.
--   If the standard versions are passed an empty list then we don't
--   get a proper source location.
module DDC.Data.ListUtils
        ( takeHead
        , takeTail
        , takeInit
        , takeMaximum
        , index)
where


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

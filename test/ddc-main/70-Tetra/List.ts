
-- | Basic Cons-lists.
module  Data.List 
of      Tetra

exports List (..)
        singleton
        length
        replicate, enumFromTo
        append, reverse
        map, mapS

imports Prim.Nat
imports Data.Ref
with

-- | Basic Cons-lists.
data List [a : Data] where
        Nil  :: List a
        Cons :: a -> List a -> List a


-- | Construct a list containing a single element.
singleton [a : Data] [x : a] : List a
 = Cons x Nil


-- | Take the length of a list.
length   [a : Data] (xx : List a) : Nat
 = region r {mutable} in
   do   ref     = allocRef [r] 0
        mapS (\_. modifyRef (+ 1) ref) xs
        readRef ref


-- | Construct a list of the given length, where all the elements have
--   the same value.
replicate [a : Data] (n : Nat) (x : a) : List a
 | n == 0       = Nil
 | otherwise    = Cons x (replicate (n - 1) x)


-- | Construct a range of values.
enumFromTo (n max : Nat) : List Nat
 | n >= max     = singleton n
 | otherwise    = Cons n (enumFromTo (n + 1) max)


-- | Append two lists.
append  [a : Data]
        (xx : List a) (yy : List a) : List a
 = case xx of
        Nil       -> yy
        Cons x xs -> Cons x (append xs yy)


-- | Reverse a list.
reverse [a : Data]
        (xx : List a) : List a
 = case xx of
        Nil       -> Nil
        Cons x xs -> append (reverse xs) (singleton x)


-- | Apply a pure worker function to all the elements of some list,
--   yielding a new list.
map     [a b : Data] 
        (f : a -> b) (xx : List a) : List b
 = case xx of
        Nil       -> Nil
        Cons x xs -> Cons (f x) (map f xs)


-- | Sequentially apply an effectful worker function to all the elements of
--   some list, yielding a new list.
mapS    [a b : Data] [e : Effect]
        (f : a -> S e b) (xx : List a) : S e (List b)
 = case xx of
        Nil       -> Nil
        Cons x xs -> Cons (f x) (mapS f xs)

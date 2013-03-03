module List
imports 
 addNat 
  ::    [r1 r2 r3 : Region].
        Nat r1 -(Pure | Use r3)>
        Nat r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)>
        Nat r3

 subNat 
  ::    [r1 r2 r3 : Region].
        Nat r1 -(Pure | Use r3)>
        Nat r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)>
        Nat r3

 showInt   :: [r : Region]. Nat# -> Ptr# r String#
 putStrLn  :: [r : Region]. Ptr# r String# -> Void#

with letrec


-- Constructors ---------------------------------------------------------------
-- | Construct a list containing a single element.
singleton 
        [r : Region] [a : Data]
        (x : a)         { Alloc r | Use r }
        : List r a
 = Cons [r] [a] x (Nil [r] [a] ())


-- | Construct a list containing copies of some value.
replicate
        [r1 r2 : Region] [a : Data]
        (n : Nat r1)            { Pure | Use r1 + Use r2 }
        (x : a)                 { Read r1 + Read r2 + Alloc r2 | Use r1 + Use r2}
        : List r2 a
 = letregion r3 in
   case n of
        N# n2   
         -> case eq# [Nat#] n2 0# of
                True#   -> Nil  [r2] [a] ()
                False#  -> Cons [r2] [a] x 
                                (replicate [:r3 r2 a:]
                                        (subNat [:r1 r3 r3:] n (N# [r3] 1#))
                                        x)

-- | Construct a range of Nat values
enumFromTo
        [r1 r2  : Region]
        (n      : Nat r2)       { Pure | Use r1 + Use r2 }
        (max    : Nat r2)       { Read r2 + Alloc r1 + Alloc r2 | Use r1 + Use r2 }
        : List r1 (Nat r2)
 = case n of
    N# n2
     -> case max of
         N# max2 
          -> case ge# [Nat#] n2 max2 of
                True#   -> singleton [r1] [Nat r2] n
                False#  -> Cons [r1] [Nat r2] n
                                (enumFromTo [:r1 r2:]
                                        (addNat [:r2 r2 r2:] n (N# [r2] 1#))
                                        max)


-- | O(n^2) reverse the elements in a list.
reverse [r1 r2 : Region] [a : Data]
        (xx : List r1 a)        { Read r1 + Read r2 + Alloc r2 | Use r1 + Use r2 }
        : List r2 a
 = case xx of
        Nil     -> Nil [:r2 a:] ()
        Cons x xs
         -> append [:r2 r2 a:] 
                (reverse   [:r1 r2 a:] xs)
                (singleton [:r2 a:] x)


-- | Append two lists.
append  [r1 r2 : Region] [a : Data]
        (xx : List r1 a)        { Pure | Use r1 + Use r2 }
        (yy : List r2 a)        { Read r1 + Alloc r2 | Use r1 + Use r2 + DeepUse a }
        : List r2 a
 = case xx of
        Nil     
         -> yy

        Cons x xs
         -> Cons [r2] [a] x (append [:r1 r2 a:] xs yy)


-------------------------------------------------------------------------------
-- | Take the length of a list.
length  [r1 r2 : Region] [a : Data]
        (xx : List r1 a)   { Read r1 + Read r2 + Alloc r2 | Use r1 + Use r2 }
        : Nat r2
 = length2 [:r1 r2 a:] (N# [r2] 0#) xx

length2 [r1 r2 : Region] [a : Data]
        (acc : Nat r2)     { Pure | Use r1 + Use r2 }
        (xx : List r1 a)   { Read r1 + Read r2 + Alloc r2 
                           | Use r1  + Use r2}
        : Nat r2
 = case xx of
        Nil -> acc

        Cons x xs
         -> length2 [:r1 r2 a:]
                    (addNat [:r2 r2 r2:] acc (N# [r2] 1#))
                    xs


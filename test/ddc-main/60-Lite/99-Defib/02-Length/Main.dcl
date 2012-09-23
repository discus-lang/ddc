
module Main 
imports {
        showInt   :: [r : %]. Nat# -> Ptr# r String#;
        putStrLn  :: [r : %]. Ptr# r String# -> Void#;
}
with letrec


-- | Add two natural numbers.
addNat [r1 r2 r3 : %] 
        (x : Nat r1)    { !0 | Use r3 }
        (y : Nat r2)    { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3 }
        : Nat r3
 = case x of
        N# n1 
         -> case y of
             N# n2 -> N# [r3] (add# [Nat#] n1 n2)


-- | Subtract two natural numbers.
subNat [r1 r2 r3 : %] 
        (x : Nat r1)    { !0 | Use r3 }
        (y : Nat r2)    { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3 }
        : Nat r3
 = case x of
        N# n1 
         -> case y of
             N# n2 -> N# [r3] (sub# [Nat#] n1 n2)
             

-- | Construct a list containing copies of some value.
replicate
        [r1 r2 : %] [a : *]
        (n : Nat r1)            { !0 | Use r1 + Use r2 }
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

-- | Take the length of a list.
length  [r1 r2 : %] [a : *]
        (xx : List r1 a)        { Read r1 + Read r2 + Alloc r2 | Use r1 + Use r2 }
        : Nat r2
 = case xx of
        Nil     
         -> N# [r2] 0#

        Cons x xs       
         -> addNat [:r2 r2 r2:] (N# [r2] 1#)
                (length [:r1 r2:] [a] xs)


-- | Construct a list of length 23 then take its length.
main    [r : %] 
        (argc : Nat#)
        (argv : Ptr# r String#)
        : Int#
 = letregion r2 in
   do
        xs      = replicate [:r2 r2:] [Nat r2] (N# [r2] 23#) (N# [r2] 100#)
        case length [:r2 r2:] [Nat r2] xs of
         N# n 
          -> do putStrLn [r2] (showInt [r2] n)
                0i#


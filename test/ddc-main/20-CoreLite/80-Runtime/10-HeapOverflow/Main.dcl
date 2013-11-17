
-- | Construct a list of 1M elements.
--   With a small fixed sized heap this will run out of space.
module Main 
exports {
        main      :: [r : Region]. Nat# -> Ptr# r String# -> Int#;
}
with letrec

-- | Subtract two natural numbers.
subNat [r1 r2 r3 : Region] 
        (x : Nat r1)    { Pure | Use r3 }
        (y : Nat r2)    { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3 }
        : Nat r3
 = case x of
        N# n1 
         -> case y of
             N# n2 -> N# [r3] (sub# [Nat#] n1 n2)


-- | Construct a list containing copies of some value.
replicate
        [r1 r2 : Region] [a : Data]
        (n : Nat r1)            { Pure | Use r1 + Use r2 }
        (x : a)                 { Read r1 + Alloc r2 | Use r1 + Use r2}
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

main    [r : Region] 
        (argc : Nat#)
        (argv : Ptr# r String#)
        : Int#
 = letregion r2 in
   do   xs1     = replicate  [:r2 r2:] [Nat r2] 
                        (N# [r2] 100000#) 
                        (N# [r2] 5#)
        0i#



module Main
exports
 main 
  ::    [r : Region]. 
        Nat# -(Pure | Use r)> 
        Ptr# r String# -(Read r + Alloc r + Console | Use r)> 
        Int#

imports
 showNat
  ::    [r : Region]. Nat# -> Ptr# r String#

 putStrLn  
  ::    [r : Region]. Ptr# r String# -(Console | Empty)> Void#

 unboxBool
  ::    [r : Region]. Bool r -(Read r | Empty)> Bool#

 unboxNat 
  ::    [r : Region]. Nat r -(Read r | Empty)> Nat#

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

 eqNat 
  ::    [r1 r2 r3 : Region].
        Nat r1 -(Pure | Use r3)>
        Nat r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)>
        Bool r3

with letrec


-- | Ackermann's function.
ack     [r1 r2 r3 : Region]
        (m : Nat r1)    {Pure | Use r1 + Use r2 + Use r3}
        (n : Nat r2)    {Read r1 + Read r2 + Alloc r3 | Use r1 + Use r2 + Use r3}
        : Nat r3
 = letregion r4 in
   case unboxBool [r4] (eqNat [:r1 r4 r4:] m (N# [r4] 0#)) of
    True#  -> addNat [:r2 r4 r3:] n (N# [r4] 1#)
    False# -> case unboxBool [r4] (eqNat [:r2 r4 r4:] n (N# [r4] 0#)) of
               True#  -> ack [:r4 r4 r3:] 
                                (subNat [:r1 r4 r4:] m (N# [r4] 1#)) 
                                (N# [r4] 1#)
               False# -> ack [:r4 r4 r3:]
                                (subNat [:r1 r4 r4:] m (N# [r4] 1#))
                                (ack [:r1 r4 r4:] m (subNat [:r2 r4 r4:] n (N# [r4] 1#)))


main    [r : Region] 
        (argc : Nat#)           {Pure                       | Use r} 
        (argv : Ptr# r String#) {Read r + Alloc r + Console | Use r} 
        : Int#
 = do   x        = ack [:r r r:] (N# [r] 3#) (N# [r] 2#)
        putStrLn [r] (showNat [r] (unboxNat [r] x))
        0i#

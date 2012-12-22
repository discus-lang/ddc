
module Main
exports
 main 
  ::    [r : %]. 
        Nat# -(!0 | Use r)> 
        Ptr# r String# -(Read r + Alloc r + Console | Use r)> 
        Int#

imports
 showNat
  ::    [r : %]. Nat# -> Ptr# r String#

 putStrLn  
  ::    [r : %]. Ptr# r String# -(Console | $0)> Void#

 unboxBool
  ::    [r : %]. Bool r -(Read r | $0)> Bool#

 unboxNat 
  ::    [r : %]. Nat r -(Read r | $0)> Nat#

 addNat 
  ::    [r1 r2 r3 : %].
        Nat r1 -(!0 | Use r3)>
        Nat r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)>
        Nat r3

 subNat 
  ::    [r1 r2 r3 : %].
        Nat r1 -(!0 | Use r3)>
        Nat r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)>
        Nat r3

 eqNat 
  ::    [r1 r2 r3 : %].
        Nat r1 -(!0 | Use r3)>
        Nat r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)>
        Bool r3

with letrec


-- | Ackermann's function.
ack     [r1 r2 r3 : %]
        (m : Nat r1)    {!0 | Use r1 + Use r2 + Use r3}
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


main    [r : %] 
        (argc : Nat#)           {!0                             | Use r} 
        (argv : Ptr# r String#) {Read r + Alloc r + Console     | Use r} 
        : Int#
 = do   x        = ack [:r r r:] (N# [r] 3#) (N# [r] 2#)
        putStrLn [r] (showNat [r] (unboxNat [r] x))
        0i#


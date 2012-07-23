module Nat with letrec


-- | Box an natural.
boxNat  [r : %] 
        (i : Nat#) { Alloc r | Use r } 
        : Nat r
 = N# [r] i


-- | Unbox an natural.
unboxNat [r : %]
        (x : Nat r) { Read r | $0 } 
        : Nat#
 = case x of 
    N# i  -> i


-- | Add two naturals.
addNat  [r1 r2 r3 : %] 
        (x : Nat r1) { !0 | Use r3 } 
        (y : Nat r2) { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3 }
        : Nat r3
 =  case x of { N# i1 
 -> case y of { N# i2 
 -> N# [r3] (add# [Nat#] i1 i2) } }


-- | Subtract the second natural from the first.
subNat  [r1 r2 r3 : %] 
        (x : Nat r1) { !0 | Use r3 } 
        (y : Nat r2) { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3 }
        : Nat r3
 =  case x of { N# i1 
 -> case y of { N# i2 
 -> N# [r3] (sub# [Nat#] i1 i2) } }


-- | Multiply two naturals.
mulNat  [r1 r2 r3 : %] 
        (x : Nat r1) { !0 | Use r3 } 
        (y : Nat r2) { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3 }
        : Nat r3
 =  case x of { N# i1 
 -> case y of { N# i2 
 -> N# [r3] (mul# [Nat#] i1 i2) } }




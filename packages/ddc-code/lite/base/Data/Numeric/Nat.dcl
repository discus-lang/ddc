module Nat 
exports
 boxNat
  ::    [r : %].
        Nat# -(Alloc r | Use r)>
        Nat r

 unboxNat 
  ::    [r : %].
        Nat r -(Read r | $0)>
        Nat#

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

 mulNat
  ::    [r1 r2 r3 : %].
        Nat r1 -(!0 | Use r3)>
        Nat r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)>
        Nat r3

 eqNat 
  ::    [r1 r2 r3 : %].
        Nat r1 -(!0 | Use r3)>
        Nat r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)>
        Bool r3

 neqNat 
  ::    [r1 r2 r3 : %].
        Nat r1 -(!0 | Use r3)>
        Nat r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)>
        Bool r3

with letrec


-- | Box an natural.
boxNat  [r : %] 
        (i : Nat#) { Alloc r | Use r } 
        : Nat r
 = N# [r] i


-- | Unbox an natural.
unboxNat [r : %]
        (x : Nat r) { Read r | $0 } 
        : Nat#
 = case x of { N# n -> n }


-- | Add two naturals.
addNat  [r1 r2 r3 : %] 
        (x : Nat r1) { !0 | Use r3 } 
        (y : Nat r2) { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3 }
        : Nat r3
 =  case x of { N# i1 -> case y of { N# i2 -> N# [r3] (add# [Nat#] i1 i2) } }


-- | Subtract the second natural from the first.
subNat  [r1 r2 r3 : %]
        (x : Nat r1) { !0 | Use r3 } 
        (y : Nat r2) { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3 }
        : Nat r3
 =  case x of { N# i1 -> case y of { N# i2 -> N# [r3] (sub# [Nat#] i1 i2) } }


-- | Multiply two naturals.
mulNat  [r1 r2 r3 : %] 
        (x : Nat r1) { !0 | Use r3 } 
        (y : Nat r2) { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3 }
        : Nat r3
 =  case x of { N# i1 -> case y of { N# i2 -> N# [r3] (mul# [Nat#] i1 i2) } }


-- | Equality on naturals.
eqNat   [r1 r2 r3 : %]
        (x : Nat r1) { !0 | Use r3 }
        (y : Nat r2) { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3}
        : Bool r3
 =  case x of { N# n1 -> case y of { N# n2 -> B# [r3] (eq# [Nat#] n1 n2) } }


-- | Negated Equality on naturals.
neqNat   [r1 r2 r3 : %]
        (x : Nat r1) { !0 | Use r3 }
        (y : Nat r2) { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3}
        : Bool r3
 =  case x of { N# n1 -> case y of { N# n2 -> B# [r3] (neq# [Nat#] n1 n2) } }


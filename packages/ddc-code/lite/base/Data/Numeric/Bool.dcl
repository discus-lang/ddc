module Bool
exports
 boxBool
  ::    [r : %].
        Bool# -(Alloc r | Use r)>
        Bool r

 unboxBool
  ::    [r : %].
        Bool r -(Read r | $0)>
        Bool#

 addBool
  ::    [r1 r2 r3 : %].
        Bool r1 -(!0 | Use r3)>
        Bool r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)>
        Bool r3

 mulBool
  ::    [r1 r2 r3 : %].
        Bool r1 -(!0 | Use r3)>
        Bool r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)>
        Bool r3

 not    
  ::    [r1 r2 : %].
        Bool r1 -(Read r1 + Alloc r2 | Use r1 + Use r2)>
        Bool r2

 and    
  ::    [r1 r2 r3 : %].
        Bool r1 -(!0 | Use r1 + Use r2)>
        Bool r2 -(Read r1 + Alloc r2 | Use r1 + Use r2)>
        Bool r2

 or
  ::    [r1 r2 r3 : %].
        Bool r1 -(!0 | Use r1 + Use r2)>
        Bool r2 -(Read r1 + Alloc r2 | Use r1 + Use r2)>
        Bool r2

with letrec


-- | Box a boolean.
boxBool [r : %] 
        (i : Bool#)     { Alloc r | Use r } 
        : Bool r
 = B# [r] i


-- | Unbox a boolean.
unboxBool [r : %]
        (x : Bool r)    { Read r | $0 } 
        : Bool#
 = case x of 
    B# i  -> i


-- | Add two booleans.
addBool [r1 r2 r3 : %] 
        (x : Bool r1)   { !0 | Use r3 } 
        (y : Bool r2)   { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3 }
        : Bool r3
 =  case x of { B# i1 
 -> case y of { B# i2 
 -> B# [r3] (add# [Bool#] i1 i2) } }


-- | Multiply two naturals.
mulBool [r1 r2 r3 : %] 
        (x : Bool r1)   { !0 | Use r3 } 
        (y : Bool r2)   { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3 }
        : Bool r3
 =  case x of { B# i1 
 -> case y of { B# i2 
 -> B# [r3] (mul# [Bool#] i1 i2) } }


-- | Boolean negation.
not     [r1 r2 : %]
        (x : Bool r1)   {Read r1 + Alloc r2 | Use r1 + Use r2}
        : Bool r2
 = case unboxBool [r1] x of
        False#  -> B# [r2] True#
        True#   -> B# [r2] False#


-- | Right biased short-circuiting and.
and     [r1 r2 r3 : %]
        (x : Bool r1)   {!0 | Use r1 + Use r2 }
        (y : Bool r2)   { Read r1 + Alloc r2 | Use r1 + Use r2 }
        : Bool r2
 = case unboxBool [r1] x of 
        False#  -> B# [r2] False#
        True#   -> y


-- | Right biased short-circuiting or.
or      [r1 r2 r3 : %]
        (x : Bool r1)   {!0 | Use r1 + Use r2 }
        (y : Bool r2)   { Read r1 + Alloc r2 | Use r1 + Use r2 }
        : Bool r2
 = case unboxBool [r1] x of 
        True#   -> B# [r2] True#
        False#  -> y

module Bool
exports
 boxBool
  ::    [r : Region].
        Bool# -(Alloc r | Use r)>
        Bool r

 unboxBool
  ::    [r : Region].
        Bool r -(Read r | Empty)>
        Bool#

 addBool
  ::    [r1 r2 r3 : Region].
        Bool r1 -(Pure | Use r3)>
        Bool r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)>
        Bool r3

 mulBool
  ::    [r1 r2 r3 : Region].
        Bool r1 -(Pure | Use r3)>
        Bool r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)>
        Bool r3

 not    
  ::    [r1 r2 : Region].
        Bool r1 -(Read r1 + Alloc r2 | Use r1 + Use r2)>
        Bool r2

 and    
  ::    [r1 r2 r3 : Region].
        Bool r1 -(Pure | Use r1 + Use r2)>
        Bool r2 -(Read r1 + Alloc r2 | Use r1 + Use r2)>
        Bool r2

 or
  ::    [r1 r2 r3 : Region].
        Bool r1 -(Pure | Use r1 + Use r2)>
        Bool r2 -(Read r1 + Alloc r2 | Use r1 + Use r2)>
        Bool r2

with letrec


-- | Box a boolean.
boxBool [r : Region] 
        (i : Bool#)     { Alloc r | Use r } 
        : Bool r
 = B# [r] i


-- | Unbox a boolean.
unboxBool 
        [r : Region]
        (x : Bool r)    { Read r | Empty } 
        : Bool#
 = case x of 
    B# i  -> i


-- | Add two booleans.
addBool [r1 r2 r3 : Region] 
        (x : Bool r1)   { Pure | Use r3 } 
        (y : Bool r2)   { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3 }
        : Bool r3
 =  case x of { B# i1 
 -> case y of { B# i2 
 -> B# [r3] (add# [Bool#] i1 i2) } }


-- | Multiply two naturals.
mulBool [r1 r2 r3 : Region] 
        (x : Bool r1)   { Pure | Use r3 } 
        (y : Bool r2)   { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3 }
        : Bool r3
 =  case x of { B# i1 
 -> case y of { B# i2 
 -> B# [r3] (mul# [Bool#] i1 i2) } }


-- | Boolean negation.
not     [r1 r2 : Region]
        (x : Bool r1)   {Read r1 + Alloc r2 | Use r1 + Use r2}
        : Bool r2
 = case unboxBool [r1] x of
        False#  -> B# [r2] True#
        True#   -> B# [r2] False#


-- | Right biased short-circuiting and.
and     [r1 r2 r3 : Region]
        (x : Bool r1)   { Pure | Use r1 + Use r2 }
        (y : Bool r2)   { Read r1 + Alloc r2 | Use r1 + Use r2 }
        : Bool r2
 = case unboxBool [r1] x of 
        False#  -> B# [r2] False#
        True#   -> y


-- | Right biased short-circuiting or.
or      [r1 r2 r3 : Region]
        (x : Bool r1)   { Pure | Use r1 + Use r2 }
        (y : Bool r2)   { Read r1 + Alloc r2 | Use r1 + Use r2 }
        : Bool r2
 = case unboxBool [r1] x of 
        True#   -> B# [r2] True#
        False#  -> y

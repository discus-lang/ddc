module Int 
exports {

boxInt  ::
    [r : Region].
    Int# -(Alloc r | Use r)>
    Int r;

unboxInt ::
    [r : Region].
    Int r -(Read r | Empty)>
    Int#;

addInt ::
    [r1 r2 r3 : Region].
    Int r1 -(Pure | Use r1 + Use r2 + Use r3)>
    Int r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r2 + Use r3)>
    Int r3;

subInt ::
    [r1 r2 r3 : Region].
    Int r1 -(Pure | Use r1 + Use r2 + Use r3)>
    Int r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r2 + Use r3)>
    Int r3;

mulInt ::
    [r1 r2 r3 : Region].
    Int r1 -(Pure | Use r1 + Use r2 + Use r3)>
    Int r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r2 + Use r3)>
    Int r3;

fac     ::
    [r : Region].
    Const r =>
    Int r -(Pure | Use r)>
    Int r -(Read r + Alloc r | Use r)>
    Int r;
}
with letrec


-- | Box an integer.
boxInt  [r : Region] 
        (i : Int#) { Alloc r | Use r } 
        : Int r
 = I# [r] i


-- | Unbox an integer.
unboxInt [r : Region]
        (x : Int r) { Read r | Empty } 
        : Int#
 = case x of 
    I# i  -> i


-- | Add two integers.
addInt [r1 r2 r3 : Region] 
        (x : Int r1) { Pure | Use r1 + Use r2 + Use r3 } 
        (y : Int r2) { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r2 + Use r3 }
        : Int r3
 =  let x' = unboxInt [r1] x	in
    let y' = unboxInt [r2] y	in
	boxInt [r3] (add# [Int#] x' y')


-- | Subtract the second integer from the first.
subInt [r1 r2 r3 : Region] 
        (x : Int r1) { Pure | Use r1 + Use r2 + Use r3 } 
        (y : Int r2) { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r2 + Use r3 }
        : Int r3
 =  let x' = unboxInt [r1] x	in
    let y' = unboxInt [r2] y	in
	boxInt [r3] (sub# [Int#] x' y')


-- | Multiply two integers.
mulInt [r1 r2 r3 : Region] 
        (x : Int r1) { Pure | Use r1 + Use r2 + Use r3 } 
        (y : Int r2) { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r2 + Use r3 }
        : Int r3
 =  let x' = unboxInt [r1] x	in
    let y' = unboxInt [r2] y	in
	boxInt [r3] (mul# [Int#] x' y')

fac     [r : Region] 
	<w : Const r>
        (acc : Int r) {Pure | Use r }
        (n   : Int r) {Read r + Alloc r | Use r} : Int r
 =  case unboxInt [r] n  of { 
	0i#   -> acc;
	1i#   -> acc;
	_       -> fac [r] <w> (mulInt [:r r r:] acc n)
			       (subInt [:r r r:] n (boxInt [r] 1i#));
 }


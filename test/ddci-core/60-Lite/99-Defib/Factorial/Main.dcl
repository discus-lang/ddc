
module Main 
imports {
        showInt   :: Int# -> Ptr# String#;
        putStrLn  :: Ptr# String# -> Void#;
}
with letrec


subInt [r1 r2 r3 : %] 
        (x : Int r1) { !0 | Use r3 } 
        (y : Int r2) { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3 }
        : Int r3
 =  case x of { I# i1 
 -> case y of { I# i2 
 -> I# [r3] (sub# [Int#] i1 i2) } }


mulInt [r1 r2 r3 : %] 
        (x : Int r1) { !0 | Use r3 } 
        (y : Int r2) { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3 }
        : Int r3
 =  case x of { I# i1 
 -> case y of { I# i2 
 -> I# [r3] (mul# [Int#] i1 i2) } }


fac    [r : %] 
       (acc : Int r) {!0 | Use r}
       (n   : Int r) {Read r + Alloc r | Use r} : Int r
 =  case n of { 
        I# i -> 
         case i of {
                0i#   -> acc;
                1i#   -> acc;
                _       -> fac [r] (mulInt [:r r r:] acc n)
                                   (subInt [:r r r:] n (I# [r] 1i#));
         };
 }


unboxInt [r : %] (x : Int r) { Read r | $0 } : Int#
 = case x of 
        I# i  -> i


main (argc : Nat#) (argv : Ptr# String#) : Int#
 = letregion r in 
   let x        = fac [r] (I# [r] 1i#) (I# [r] 10i#) in
   let _        = putStrLn (showInt (unboxInt [r] x)) in
   unboxInt [r] (I# [r] 0i#)

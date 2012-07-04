
module Main 
imports {
        showInt32 :: Int32# -> Ptr# String#;
        putStrLn  :: Ptr# String# -> Void#;
}
with letrec


subInt [r1 r2 r3 : %] 
        (x : Int r1) { !0 | Use r3 } 
        (y : Int r2) { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3 }
        : Int r3
 =  case x of { I32# i1 
 -> case y of { I32# i2 
 -> I32# [r3] (sub# [Int32#] i1 i2) } }


mulInt [r1 r2 r3 : %] 
        (x : Int r1) { !0 | Use r3 } 
        (y : Int r2) { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3 }
        : Int r3
 =  case x of { I32# i1 
 -> case y of { I32# i2 
 -> I32# [r3] (mul# [Int32#] i1 i2) } }


fac    [r : %] 
       (acc : Int r) {!0 | Use r}
       (n   : Int r) {Read r + Alloc r | Use r} : Int r
 =  case n of { 
        I32# i -> 
         case i of {
                0i32#   -> acc;
                1i32#   -> acc;
                _       -> fac [r] (mulInt [:r r r:] acc n)
                                   (subInt [:r r r:] n (I32# [r] 1i32#));
         };
 }


unboxInt [r : %] (x : Int r) { Read r | $0 } : Int32#
 = case x of 
        I32# i  -> i


main (argc : Nat#) (argv : Ptr# String#) : Int32#
 = letregion r in 
   let x        = fac [r] (I32# [r] 1i32#) (I32# [r] 10i32#) in
   let _        = putStrLn (showInt32 (unboxInt [r] x)) in
   unboxInt [r] (I32# [r] 0i32#)


module Main 
imports {
        showInt32# :: Int32# -> Ptr# String#;
        putStrLn#  :: Ptr# String# -> Void#;
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
 -> I32# [r3] (sub# [Int32#] i1 i2) } }


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


--main (argc : Nat#) (argv : Ptr# String#) : Int32#
-- = letregion r in 
--   do   x       = fac [r] (0 [r] ()) (5 [r] ())
--        putStrLn# (showInt32# (unboxInt [r] x))
--        unboxInt [r] (0 [r] ())


module Main
exports 
        main    :: [r : Region]
                .  Nat# 
                -(Pure | Use r)> Ptr# r String# 
                -(Read r + Alloc r + Console | Use r)> Int#
imports 
        showInt   :: [r : Region]. Int# -> Ptr# r String#
        putStrLn  :: [r : Region]. Ptr# r String# -(Console | Empty)> Void#

with letrec


subInt  [r1 r2 r3 : Region] 
        (x : Int r1)            { Pure | Use r3 } 
        (y : Int r2)            { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3 }
        : Int r3
 =  case x of { I# i1 
 -> case y of { I# i2 
 -> I# [r3] (sub# [Int#] i1 i2) } }


mulInt  [r1 r2 r3 : Region] 
        (x : Int r1)            { Pure | Use r3 } 
        (y : Int r2)            { Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3 }
        : Int r3
 =  case x of { I# i1 
 -> case y of { I# i2 
 -> I# [r3] (mul# [Int#] i1 i2) } }


unboxInt [r : Region] (x : Int r)    { Read r | Empty } 
        : Int#
 = case x of 
        I# i  -> i


fac     [r : Region] 
        (acc : Int r)           {Pure | Use r }
        (n   : Int r)           {Read r + Alloc r | Use r} : Int r
 =  case n of { 
        I# i -> 
         case i of {
                0i#   -> acc;
                1i#   -> acc;
                _       -> fac [r] (mulInt [:r r r:] acc n)
                                   (subInt [:r r r:] n (I# [r] 1i#));
         };
 }


main    [r : Region] 
        (argc : Nat#)           {Pure                             | Use r} 
        (argv : Ptr# r String#) {Read r + Alloc r + Console     | Use r} 
        : Int#
 = do   x        = fac [r] (I# [r] 1i#) (I# [r] 10i#)
        putStrLn [r] (showInt [r] (unboxInt [r] x))
        unboxInt [r] (I# [r] 0i#)

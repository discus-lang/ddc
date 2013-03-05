
module Main
exports 
 main    
  ::    [r : Region].
        Nat#            -(Pure | Use r)> 
        Ptr# r String#  -(Read r + Alloc r + Console | Use r)> 
        Int#

imports 
 showInt
  :: [r : Region]. Int# -> Ptr# r String#

 putStrLn  
  :: [r : Region]. Ptr# r String# -(Console | Empty)> Void#

 unboxInt 
  ::    [r : Region].
        Int r -(Read r | Empty)>
        Int#

 subInt 
  ::    [r1 r2 r3 : Region].
        Int r1 -(Pure | Use r3)>
        Int r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)>
        Int r3

 mulInt 
  ::    [r1 r2 r3 : Region].
        Int r1 -(Pure | Use r3)>
        Int r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)>
        Int r3

with letrec

fac     [r : Region] 
        (acc : Int r)           {Pure | Use r }
        (n   : Int r)           {Read r + Alloc r | Use r} : Int r
 =  case n of
        I# i -> 
         case i of
                0i#   -> acc
                1i#   -> acc
                _       -> fac [r] (mulInt [:r r r:] acc n)
                                   (subInt [:r r r:] n (I# [r] 1i#))


main    [r : Region] 
        (argc : Nat#)           {Pure                       | Use r} 
        (argv : Ptr# r String#) {Read r + Alloc r + Console | Use r} 
        : Int#
 = do   x        = fac [r] (I# [r] 1i#) (I# [r] 10i#)
        putStrLn [r] (showInt [r] (unboxInt [r] x))
        0i#


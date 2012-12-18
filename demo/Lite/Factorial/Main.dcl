
module Main
exports 
 main    
  ::    [r : %].
        Nat#            -(!0 | Use r)> 
        Ptr# r String#  -(Read r + Alloc r + Console | Use r)> 
        Int#

imports 
 showInt
  :: [r : %]. Int# -> Ptr# r String#

 putStrLn  
  :: [r : %]. Ptr# r String# -(Console | $0)> Void#

 unboxInt 
  ::    [r : %].
        Int r -(Read r | $0)>
        Int#

 subInt 
  ::    [r1 r2 r3 : %].
        Int r1 -(!0 | Use r3)>
        Int r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)>
        Int r3

 mulInt 
  ::    [r1 r2 r3 : %].
        Int r1 -(!0 | Use r3)>
        Int r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)>
        Int r3

with letrec

fac     [r : %] 
        (acc : Int r)           {!0 | Use r }
        (n   : Int r)           {Read r + Alloc r | Use r} : Int r
 =  case n of
        I# i -> 
         case i of
                0i#   -> acc
                1i#   -> acc
                _       -> fac [r] (mulInt [:r r r:] acc n)
                                   (subInt [:r r r:] n (I# [r] 1i#))


main    [r : %] 
        (argc : Nat#)           {!0                             | Use r} 
        (argv : Ptr# r String#) {Read r + Alloc r + Console     | Use r} 
        : Int#
 = do   x        = fac [r] (I# [r] 1i#) (I# [r] 10i#)
        putStrLn [r] (showInt [r] (unboxInt [r] x))
        0i#


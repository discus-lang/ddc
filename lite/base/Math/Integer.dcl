
module Integer 
imports {
        subInt  :: [r1 r2 r3 : %]
                . Int r1 -(!0 | Use r3)> 
                  Int r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)> 
                  Int r3;

        mulInt  :: [r1 r2 r3 : %]
                . Int r1 -(!0 | Use r3)> 
                  Int r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)> 
                  Int r3;
}
with letrec

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

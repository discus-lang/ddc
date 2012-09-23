
module List
imports {
        subNat  :: [r1 r2 r3 : %]
                . Nat r1 
                -(!0 | Use r3)> Nat r2 
                -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)> Nat r3;
}
with letrec

replicate
        [r1 r2 : %] [a : *]
        (n : Nat r1)            { !0 | Use r1 + Use r2 }
        (x : a)                 { Read r1 + Read r2 + Alloc r2 | Use r1 + Use r2}
        : List r2 a
 = letregion r3 in
   case n of
        N# n2   
         -> case eq# [Nat#] n2 0# of
                True#   -> Nil  [r2] [a] ()
                False#  -> Cons [r2] [a] x 
                                (replicate [:r3 r2 a:]
                                        (subNat [:r1 r3 r3:] n (N# [r3] 1#))
                                        x)

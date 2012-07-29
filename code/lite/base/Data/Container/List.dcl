module List
imports {
        addNat  :: [r1 r2 r3 : %]
                . Nat r1 
                -(!0 | Use r3)> Nat r2 
                -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)> Nat r3;

        subNat  :: [r1 r2 r3 : %]
                . Nat r1 
                -(!0 | Use r3)> Nat r2 
                -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)> Nat r3;

        showInt   :: [r : %]. Nat# -> Ptr# r String#;
        putStrLn  :: [r : %]. Ptr# r String# -> Void#;

}
with letrec


-- Constructors ---------------------------------------------------------------
-- | Construct a list containing a single element.
singleton 
        [r : %] [a : *]
        (x : a)         { Alloc r | Use r }
        : List r a
 = Cons [r] [a] x (Nil [r] [a] ())


-- | Construct a list containing copies of some value.
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

-- | Append two lists.
append  [r1 r2 : %] [a : *]
        (xx : List r1 a)        { !0 | Use r1 + Use r2 }
        (yy : List r2 a)        { Read r1 + Alloc r2 | Use r1 + Use r2 + DeepUse a }
        : List r2 a
 = case xx of
        Nil     
         -> yy

        Cons x xs
         -> Cons [r2] [a] x (append [:r1 r2 a:] xs yy)


-------------------------------------------------------------------------------
-- | Take the length of a list.
length  [r1 r2 : %] [a : *]
        (xx : List r1 a)        { Read r1 + Read r2 + Alloc r2 | Use r1 + Use r2 }
        : Nat r2
 = case xx of
        Nil     
         -> N# [r2] 0#

        Cons x xs       
         -> addNat [:r2 r2 r2:] (N# [r2] 1#)
                (length [:r1 r2:] [a] xs)
        

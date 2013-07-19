module Main 
exports {
        main :: [r : Region].Nat# -> Ptr# r String# -(Console | Empty)> Int#;
} 
imports {
        addNat :: [r1 r2 r3 : Region].Nat r1 -(Pure | Use r3)> Nat r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)> Nat r3;
        boxNat :: [r : Region].Nat# -(Alloc r | Use r)> Nat r;
        putStrLn :: [r : Region].Ptr# r String# -(Console | Empty)> Void#;
        showNat :: [r : Region].Nat# -> Ptr# r String#;
        subNat :: [r1 r2 r3 : Region].Nat r1 -(Pure | Use r3)> Nat r2 -(Read r1 + Read r2 + Alloc r3 | Use r1 + Use r3)> Nat r3;
        unboxNat :: [r : Region].Nat r -(Read r | Empty)> Nat#;
} with
letrec {
  singleton : [r : Region].[a : Data].a -(Alloc r | Use r)> List r a
    = /\(r : Region)./\(a : Data).
       \(x : a).
      let x0 : List r a = Nil [r] [a] () in
      Cons [r] [a] x x0;
  
  length : [r1 r2 : Region].[a : Data].List r1 a -(Read r1 + Read r2 + Alloc r2 | Use r1 + Use r2)> Nat r2
    = /\(r1 r2 : Region)./\(a : Data).
       \(xx : List r1 a).
      case xx of {
        Nil  
         -> N# [r2] 0#;
        Cons (x : a) (xs : List r1 a) 
         -> let x1 : Nat r2 = N# [r2] 1# in
            let x3 : Nat r2 = length [r1] [r2] [a] xs in
            addNat [r2] [r2] [r2] x1 x3
      };
  
  replicate : [r1 r2 : Region].[a : Data].Nat r1 -(Pure | Use r1 + Use r2)> a -(Read r1 + Alloc r2 | Use r1 + Use r2)> List r2 a
    = /\(r1 r2 : Region)./\(a : Data).
       \(n : Nat r1).\(x : a).
      letregion r3 in
      case n of {
        N# (n2 : Nat#) 
         -> let x4 : Bool# = eq# [Nat#] n2 0# in
            case x4 of {
              True#  
               -> Nil [r2] [a] ();
              False#  
               -> let x5 : Nat r3 = N# [r3] 1# in
                  let x6 : Nat r3 = subNat [r1] [r3] [r3] n x5 in
                  let x7 : List r2 a = replicate [r3] [r2] [a] x6 x in
                  Cons [r2] [a] x x7
            }
      };
  
  enumFromTo : [r1 r2 : Region].Nat r2 -(Pure | Use r1 + Use r2)> Nat r2 -(Read r2 + Alloc r1 + Alloc r2 | Use r1 + Use r2)> List r1 (Nat r2)
    = /\(r1 r2 : Region).
       \(n max : Nat r2).
      case n of {
        N# (n2 : Nat#) 
         -> case max of {
              N# (max2 : Nat#) 
               -> let x8 : Bool# = ge# [Nat#] n2 max2 in
                  case x8 of {
                    True#  
                     -> singleton [r1] [Nat r2] n;
                    False#  
                     -> let x9 : Nat r2 = N# [r2] 1# in
                        let x10 : Nat r2 = addNat [r2] [r2] [r2] n x9 in
                        let x11 : List r1 (Nat r2) = enumFromTo [r1] [r2] x10 max in
                        Cons [r1] [Nat r2] n x11
                  }
            }
      };
  
  append : [r1 r2 : Region].[a : Data].List r1 a -(Pure | Use r1 + Use r2)> List r2 a -(Read r1 + Alloc r2 | Use r1 + Use r2 + DeepUse a)> List r2 a
    = /\(r1 r2 : Region)./\(a : Data).
       \(xx : List r1 a).\(yy : List r2 a).
      case xx of {
        Nil  
         -> yy;
        Cons (x : a) (xs : List r1 a) 
         -> let x12 : List r2 a = append [r1] [r2] [a] xs yy in
            Cons [r2] [a] x x12
      };
  
  reverse : [r1 r2 : Region].[a : Data].List r1 a -(Read r1 + Read r2 + Alloc r2 | Use r1 + Use r2)> List r2 a
    = /\(r1 r2 : Region)./\(a : Data).
       \(xx : List r1 a).
      case xx of {
        Nil  
         -> Nil [r2] [a] ();
        Cons (x : a) (xs : List r1 a) 
         -> let x22 : List r2 a = reverse [r1] [r2] [a] xs in
            let x23 : List r2 a = singleton [r2] [a] x in
            case x22 of {
              Nil  
               -> x23;
              Cons (x18 : a) (x19 : List r2 a) 
               -> let x20 : List r2 a = append [r2] [r2] [a] x19 x23 in
                  Cons [r2] [a] x18 x20
            }
      };
  
  dumpNats : [r1 r2 : Region].List r1 (Nat r2) -(Read r1 + Read r2 + Console | Use r1 + Use r2)> Unit
    = /\(r1 r2 : Region).
       \(xx : List r1 (Nat r2)).
      case xx of {
        Nil  
         -> ();
        Cons (x : Nat r2) (xs : List r1 (Nat r2)) 
         -> case x of {
              N# (x2 : Nat#) 
               -> let x24 : Ptr# r2 String# = showNat [r2] x2 in
                  let _ : Void# = putStrLn [r2] x24 in
                  dumpNats [r1] [r2] xs
            }
      };
  
  main : [r : Region].Nat# -> Ptr# r String# -(Console | Empty)> Int#
    = /\(r : Region).
       \(argc : Nat#).\(argv : Ptr# r String#).
      letregion r2 in
      let x25 : Nat r2 = N# [r2] 5# in
      let x26 : Nat r2 = N# [r2] 100# in
      let xs1 : List r2 (Nat r2)
            = replicate [r2] [r2] [Nat r2] x25 x26 in
      let x27 : Nat r2 = N# [r2] 10# in
      let x28 : Nat r2 = N# [r2] 20# in
      let xs2 : List r2 (Nat r2) = enumFromTo [r2] [r2] x27 x28 in
      let zs : List r2 (Nat r2)
            = case xs1 of {
                Nil  
                 -> xs2;
                Cons (x34 : Nat r2) (x35 : List r2 (Nat r2)) 
                 -> let x36 : List r2 (Nat r2)
                          = append [r2] [r2] [Nat r2] x35 xs2 in
                    Cons [r2] [Nat r2] x34 x36
              } in
      let zs2 : List r2 (Nat r2)
            = reverse [r2] [r2] [Nat r2] zs in
      let _ : Unit = dumpNats [r2] [r2] zs2 in
      0i#
}

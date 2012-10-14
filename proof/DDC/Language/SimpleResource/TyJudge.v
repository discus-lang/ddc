
Require Import DDC.Language.SimpleResource.Ty.
Require Import DDC.Language.SimpleResource.Exp.

Fixpoint substTT (d: nat) (t1: ty) (t2: ty)
 := t2.

Fixpoint substRT (d: nat) (r1: res) (t2: ty)
 := t2.

Inductive TYPE (kn: nat) (rn: nat) (te: list ty) 
               : exp -> ty -> res -> res -> Prop :=

 | TyVar
   :  forall vi t1
   ,  get vi te = Some t1
   -> TYPE kn rn te (XVar vi) t1 RZero RZero

 (* Type Abstraction / Application *)
 | TyLamT 
   :  forall x1 t1
   ,  TYPE (S kn) rn te x1 t1                 RZero RZero
   -> TYPE kn rn  te (XLamT x1) (TForallT t1) RZero RZero

 | TyAppT 
   :  forall x1 t11 t2
   ,  TYPE kn rn te x1 (TForallT t11)                RZero RZero
   -> wfT  kn rn t2
   -> TYPE kn rn te (XAppT x1 t2) (substTT 0 t2 t11) RZero RZero

 (* Resource Abstraction / Application *)
 | TyLamR 
   :  forall x1 t1
   ,  TYPE kn (S rn) te x1 t1                        RZero RZero
   -> TYPE kn rn te  (XLamR x1) (TForallR t1)        RZero RZero

 | TyAppR 
   :  forall x1 t11 r2
   ,  TYPE kn rn te x1 (TForallR t11)                RZero RZero
   -> wfR  kn r2
   -> TYPE kn rn te (XAppR x1 r2) (substRT 0 r2 t11) RZero RZero

 (* Value Abstraction / Application *)
 | TyLam
   :  forall x2 t1 t2 w s
   ,  TYPE kn rn (te :> t1) x2 t2 w s
   -> wfT  kn rn t1
   -> TYPE kn rn te (XLam t1 x2) (TFun t1 t2 w s)    RZero RZero

 | TyApp
   :  forall x1 x2 t11 t12 w1 s1 w2 s2 w3 s3
   ,  TYPE kn rn te x1 (TFun t11 t12 w3 s3) w1 s1
   -> TYPE kn rn te x2 t11 w2 s2
   -> TYPE kn rn te (XApp x1 x2) t12 
           (RAdd w1 (RAdd w2 w3))
           (RAdd s1 (RAdd s2 s3)).

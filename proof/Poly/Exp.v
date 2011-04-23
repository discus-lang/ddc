
Require Import Ty.
Require Import Env.
Require Import Base.


(** Expressions *****************************************************)
Inductive exp : Type :=
 | XVar  : nat -> exp
 | XLAM  : exp -> exp
 | XAPP  : exp -> ty  -> exp
 | XLam  : ty  -> exp -> exp
 | XApp  : exp -> exp -> exp.
Hint Constructors exp.


(* Closedness under type environment ********************************)

(* Exp is val closed under a type environment of a given length *)
Inductive coversXX : nat -> exp -> Prop :=
 | CoversXX_var
   :  forall n i
   ,  n > i
   -> coversXX n (XVar i)

 | CoversXX_LAM
   :  forall n x
   ,  coversXX n x
   -> coversXX n (XLAM x)

 | CoversXX_APP 
   :  forall n x t
   ,  coversXX n x
   -> coversXX n (XAPP x t)

 | CoversXX_lam
   :  forall n t x
   ,  coversXX (S n) x
   -> coversXX n (XLam t x)

 | CoversXX_app
   :  forall n x1 x2
   ,  coversXX n x1 -> coversXX n x2
   -> coversXX n (XApp x1 x2).
Hint Constructors coversXX.


(* Exp is val closed under the given type environment *)
Inductive closedUnderXX : tyenv -> exp -> Prop :=
  |  ClosedUnderXX 
     :  forall tenv x
     ,  coversXX (length tenv) x
     -> closedUnderXX tenv x.
Hint Constructors closedUnderXX.


(* Exp is val closed under the empty type environment *)
Inductive closedXX : exp -> Prop :=
 |  ClosedXX
    :  forall x
    ,  coversXX 0 x
    -> closedXX x.
Hint Constructors closedXX.


(* Closedness under kind environment ********************************)

(* Exp is type closed under a kind environment of a given length *)
Inductive coversTX : nat -> exp -> Prop :=
 | CoversTX_var
   :  forall n i
   ,  coversTX n (XVar i)

 | CoversTX_LAM
   :  forall n x
   ,  coversTX (S n) x
   -> coversTX n (XLAM x)

 | CoversTX_APP 
   :  forall n x t
   ,  coversTX n x
   -> coversTT n t
   -> coversTX n (XAPP x t)

 | CoversTX_lam
   :  forall n t x
   ,  coversTX n x
   -> coversTX n (XLam t x)

 | CoversTX_app
   :  forall n x1 x2
   ,  coversTX n x1 -> coversTX n x2
   -> coversTX n (XApp x1 x2).
Hint Constructors coversTX.


(* Exp is type closed under the given kind environment *)
Inductive closedUnderTX : kienv -> exp -> Prop :=
  |  ClosedUnderTX 
     :  forall kenv x
     ,  coversTX (length kenv) x
     -> closedUnderTX kenv x.
Hint Constructors closedUnderTX.


(* Exp is type closed under the empty kind environment *)
Inductive closedTX : exp -> Prop :=
 |  ClosedTX
    :  forall x
    ,  coversTX 0 x
    -> closedTX x.
Hint Constructors closedTX.


(* Lemmas ***********************************************************)
Lemma coversXX_succ
 :  forall n x
 ,  coversXX n     x
 -> coversXX (S n) x.
Proof.
 intros. gen n.
 induction x; intros; inverts H; auto.
Qed.
Hint Resolve coversTT_succ.


Lemma coversTX_succ
 :  forall n x
 ,  coversTX n     x
 -> coversTX (S n) x.
Proof.
 intros. gen n.
 induction x; intros; inverts H; auto.
Qed.
Hint Resolve coversTX_succ.



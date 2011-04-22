
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
Inductive coversXT : nat -> exp -> Prop :=
 | CoversXT_var
   :  forall n i
   ,  coversXT n (XVar i)

 | CoversXT_LAM
   :  forall n x
   ,  coversXT (S n) x
   -> coversXT n (XLAM x)

 | CoversXT_APP 
   :  forall n x t
   ,  coversXT n x
   -> coversT  n t
   -> coversXT n (XAPP x t)

 | CoversXT_lam
   :  forall n t x
   ,  coversXT n x
   -> coversXT n (XLam t x)

 | CoversXT_app
   :  forall n x1 x2
   ,  coversXT n x1 -> coversXT n x2
   -> coversXT n (XApp x1 x2).
Hint Constructors coversXT.


(* Exp is type closed under the given kind environment *)
Inductive closedUnderXT : kienv -> exp -> Prop :=
  |  ClosedUnderXT 
     :  forall kenv x
     ,  coversXT (length kenv) x
     -> closedUnderXT kenv x.
Hint Constructors closedUnderXT.


(* Exp is type closed under the empty kind environment *)
Inductive closedXT : exp -> Prop :=
 |  ClosedXT
    :  forall x
    ,  coversXT 0 x
    -> closedXT x.
Hint Constructors closedXT.


(* Lemmas ***********************************************************)
Lemma coversXX_succ
 :  forall n x
 ,  coversXX n     x
 -> coversXX (S n) x.
Proof.
 intros. gen n.
 induction x; intros; inversions H; auto.
Qed.
Hint Resolve coversT_succ.


Lemma coversXT_succ
 :  forall n x
 ,  coversXT n     x
 -> coversXT (S n) x.
Proof.
 intros. gen n.
 induction x; intros; inversions H; auto.
Qed.
Hint Resolve coversXT_succ.






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


(* Closedness *******************************************************)

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






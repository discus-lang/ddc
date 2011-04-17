
Require Export Base.
Require Export Env.


(** Types *************************************************)
Inductive ty  : Type :=
 | TCon  : nat -> ty
 | TFun  : ty  -> ty -> ty.

Hint Constructors ty.


(** Type Environments *************************************)
Definition tyenv := env ty.


(** Expressions *******************************************)
Inductive exp : Type :=
 | XVar  : nat -> exp
 | XLam  : ty  -> exp -> exp
 | XApp  : exp -> exp -> exp.

Hint Constructors exp.


(** Closedness ********************************************)

(* Expression is closed under an enviornment of a given size *)
Inductive coversX : nat -> exp -> Prop :=
 | CoversX_var
   :  forall n i
   ,  (n > i) 
   -> coversX n (XVar i)

 | CoversX_lam
   :  forall n T t
   ,  coversX (S n) t 
   -> coversX n (XLam T t)

 | CoversX_app
   :  forall n t1 t2
   ,  coversX n t1
   -> coversX n t2
   -> coversX n (XApp t1 t2).
Hint Constructors coversX.


(* Expression is closed under the given environment. *)
Inductive closedUnderX : tyenv -> exp -> Prop :=
 | ClosedUnderX 
   : forall tenv x 
   , coversX (length tenv) x -> closedUnderX tenv x. 


(* Expression is closed under an empty environment, 
   it has no free locally bound varirables *)
Inductive closedX : exp -> Prop :=
 | ClosedX 
   : forall xx
   , coversX 0 xx -> closedX xx.


Inductive value : exp -> Prop :=
 | Value_lam 
   : forall T t
   , closedX (XLam T t) -> value (XLam T t).

Hint Constructors value.


(** Lemmas **********************************************************)
Lemma closed_app
 : forall t1 t2
 , closedX (XApp t1 t2) -> closedX t1 /\ closedX t2.
Proof.
 intros.
 inversions H. inversions H0.
 apply ClosedX in H3.
 apply ClosedX in H4.
 auto.
Qed.



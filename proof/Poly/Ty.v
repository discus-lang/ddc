
Require Import Env.
Require Import Base.


(* Kinds ************************************************************)
Inductive ki : Type :=
 | KStar   : ki.


(** Types ***********************************************************)
Inductive ty  : Type :=
 | TCon    : nat -> ty
 | TVar    : nat -> ty
 | TForall : ty  -> ty
 | TFun    : ty  -> ty -> ty.
Hint Constructors ty.


(* Environments *****************************************************)
Definition tyenv := env ty.
Definition kienv := env ki.


(* Closedness *******************************************************)

(* Type is closed under a kind environment of a given length. *)
Inductive coversTT : nat -> ty -> Prop :=
 | CoversTT_con 
   :  forall n c
   ,  coversTT n (TCon c)

 | CoversTT_var         
   :  forall n i
   ,  n > i 
   -> coversTT n (TVar i)

 | CoversTT_forall
   :  forall n t
   ,  coversTT (S n) t
   -> coversTT n (TForall t)

 | CoversT_fun
   :  forall n t1 t2
   ,  coversTT n t1 -> coversTT n t2
   -> coversTT n (TFun t1 t2).
Hint Constructors coversTT.


(* Type is closed under the given kind environment. *)
Inductive closedUnderTT : kienv -> ty -> Prop :=
 | ClosedUnderTT 
   :  forall kenv t
   ,  coversTT (length kenv) t
   -> closedUnderTT kenv t.


(* Type is closed under an empty kind environment. *)
Inductive closedTT : ty -> Prop :=
 | ClosedTT 
   :  forall t
   ,  coversTT 0 t
   -> closedTT t.
Hint Constructors closedTT.


(* Normal Types *****************************************************
   A normal form type is a closed type that cannot be reduced further.
 *)
Inductive normalT : ty -> Prop :=
 | NormalT_con
   : forall c
   , normalT (TCon c)

 | NormalT_forall
   :  forall t
   ,  normalT t
   -> normalT (TForall t)

 | NormalT_fun
   :  forall t1 t2
   ,  normalT t1 -> normalT t2
   -> normalT (TFun t1 t2).
Hint Constructors normalT.


(* Lemmas ***********************************************************)
Lemma coversTT_succ
 :  forall n t
 ,  coversTT n     t
 -> coversTT (S n) t.
Proof.
 intros. gen n.
 induction t; intros; inversions H; auto.
Qed.
Hint Resolve coversTT_succ.

  

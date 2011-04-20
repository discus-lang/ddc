
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
Inductive coversT : nat -> ty -> Prop :=
 | CoversT_con 
   :  forall n c
   ,  coversT n (TCon c)

 | CoversT_var         
   :  forall n i
   ,  n > i 
   -> coversT n (TVar i)

 | CoversT_forall
   :  forall n t
   ,  coversT (S n) t
   -> coversT n (TForall t)

 | CoversT_fun
   :  forall n t1 t2
   ,  coversT n t1 -> coversT n t2
   -> coversT n (TFun t1 t2).
Hint Constructors coversT.


(* Type is closed under the given kind environment. *)
Inductive closedUnderT : kienv -> ty -> Prop :=
 | ClosedUnderT 
   :  forall kenv t
   ,  coversT (length kenv) t
   -> closedUnderT kenv t.


(* Type is closed under an empty kind environment. *)
Inductive closedT : ty -> Prop :=
 | ClosedT 
   :  forall t
   ,  coversT 0 t
   -> closedT t.
Hint Constructors closedT.


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
Lemma coversT_succ
 :  forall n t
 ,  coversT n     t
 -> coversT (S n) t.
Proof.
 intros. gen n.
 induction t; intros; inversions H; auto.
Qed.
Hint Resolve coversT_succ.

  



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


(* Well Formedness **************************************************)

(* Well formed types are closed under the given kind environment *)
Fixpoint wfT (kenv: kienv) (tt: ty) : Prop := 
 match tt with
 | TCon _     => True
 | TVar it    => exists k, get kenv it = Some k
 | TForall t  => wfT (kenv :> KStar) t
 | TFun t1 t2 => wfT kenv t1 /\ wfT kenv t2
 end.
Hint Unfold wfT.


(* Type is closed under an empty kind environment. *)
Definition closedT (tt: ty) : Prop
 := wfT Empty tt.
Hint Unfold closedT.


  

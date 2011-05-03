
Require Export Base.
Require Export Env.


(** Types ***********************************************************)
Inductive ty  : Type :=
 | TCon  : nat -> ty
 | TFun  : ty  -> ty -> ty.
Hint Constructors ty.


(** Expressions *****************************************************)
Inductive exp : Type :=
 | XVar  : nat -> exp
 | XLam  : ty  -> exp -> exp
 | XApp  : exp -> exp -> exp.
Hint Constructors exp.


(** Environments ****************************************************)
Definition tyenv := env ty.


(** Well Formedness *************************************************)
(* Well formed expressions are closed under the given environment *)
Fixpoint wfX (tenv: tyenv) (xx: exp) : Prop :=
 match xx with 
 | XVar i     => exists t, get tenv i = Some t
 | XLam t x   => wfX (tenv :> t) x
 | XApp x1 x2 => wfX tenv x1 /\ wfX tenv x2
 end.


(* Closed expressions are well formed under an empty environment *)
Definition closedX (xx: exp) : Prop
 := wfX Empty xx.
Hint Unfold closedX.


(* Values are closed expressions that cannot be reduced further. *)
Inductive value : exp -> Prop :=
 | Value_lam 
   : forall t x
   , closedX (XLam t x) -> value (XLam t x).
Hint Constructors value.


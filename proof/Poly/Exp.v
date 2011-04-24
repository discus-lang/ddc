
Require Import Ty.
Require Import Env.
Require Import Base.
Require Import SubstTypeType.

(** Expressions *****************************************************)
Inductive exp : Type :=
 | XVar  : nat -> exp
 | XLAM  : exp -> exp
 | XAPP  : exp -> ty  -> exp
 | XLam  : ty  -> exp -> exp
 | XApp  : exp -> exp -> exp.
Hint Constructors exp.


(* Well Formedness **************************************************)

(* Exp is closed under the given environments *)
Fixpoint wfX (kenv: env ki) (tenv: env ty) (xx: exp) : Prop := 
 match xx with 
 | XVar i     => exists t, get tenv i = Some t
 | XLAM x     => wfX (kenv :> KStar) tenv x
 | XAPP x t   => wfX kenv tenv x  /\ wfT kenv t
 | XLam t x   => wfT kenv t       /\ wfX kenv (tenv :> t) x
 | XApp x1 x2 => wfX kenv tenv x1 /\ wfX kenv tenv x2
 end.
Hint Unfold wfX.


(* Closed expressions are well formed under empty environments *)
Definition closedX (xx: exp) : Prop
 := wfX Empty Empty xx.
Hint Unfold closedX.


(* Values are closed expressions that cannot be reduced further *)
Inductive value : exp -> Prop :=
 | Value_lam 
   : forall t x
   , closedX (XLam t x) -> value (XLam t x).
Hint Constructors value.



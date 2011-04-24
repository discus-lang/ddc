
Require Import Exp.
Require Import TyEnv.


(* Types ************************************************************)
(* Well formed types are closed under the given kind environment *)
Fixpoint wfT (e: tyenv) (tt: ty) : Prop := 
 match tt with
 | TCon _     => True
 | TVar i     => exists k, getK e i = Some k
 | TForall t  => wfT (e :> EKind KStar) t
 | TFun t1 t2 => wfT e t1 /\ wfT e t2
 end.
Hint Unfold wfT.


(* Type is closed under an empty kind environment. *)
Definition closedT (tt: ty) : Prop
 := wfT Empty tt.
Hint Unfold closedT.


(* Environments ****************************************************)
(* All types in a well formed environment are well formed under
   the subsequent environment 
 *)
Fixpoint wfEnv (e: tyenv) : Prop :=
 match e with 
 | Empty         => True
 | e' :> EKind _ => wfEnv e'
 | e' :> EType t => wfT e' t /\ wfEnv e'
 end.


(* Expressions ******************************************************)
(* A well formed expression is closed under the given environment *)
Fixpoint wfX (e: tyenv) (xx: exp) : Prop := 
 match xx with 
 | XVar i     => exists t, getT e i = Some t
 | XLAM x     => wfX (e :> EKind KStar) x
 | XAPP x t   => wfX e x  /\ wfT e t
 | XLam t x   => wfT e t  /\ wfX (e :> EType t) x
 | XApp x1 x2 => wfX e x1 /\ wfX e x2
 end.
Hint Unfold wfX.


(* Closed expressions are well formed under empty environments *)
Definition closedX (xx: exp) : Prop
 := wfX Empty xx.
Hint Unfold closedX.


(* Values are closed expressions that cannot be reduced further *)
Inductive value : exp -> Prop :=
 | Value_lam 
   : forall t x
   , closedX (XLam t x) -> value (XLam t x).
Hint Constructors value.



Require Import Exp.


(* Types ************************************************************)
(* Well formed types are closed under the given kind environment *)
Fixpoint wfT (ke: kienv) (tt: ty) : Prop := 
 match tt with
 | TCon _     => True
 | TVar i     => exists k, get ke i = Some k
 | TForall t  => wfT (ke :> KStar) t
 | TFun t1 t2 => wfT ke t1 /\ wfT ke t2
 end.
Hint Unfold wfT.


(* Type is closed under an empty kind environment. *)
Definition closedT (tt: ty) : Prop
 := wfT Empty tt.
Hint Unfold closedT.


(* Expressions ******************************************************)
(* A well formed expression is closed under the given environments *)
Fixpoint wfX (ke: kienv) (te: tyenv) (xx: exp) : Prop := 
 match xx with 
 | XVar i     => exists t, get te i = Some t
 | XLAM x     => wfX (ke :> KStar) te x
 | XAPP x t   => wfX ke te x  /\ wfT ke t
 | XLam t x   => wfT ke t     /\ wfX ke (te :> t) x
 | XApp x1 x2 => wfX ke te x1 /\ wfX ke te x2
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


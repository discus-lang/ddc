
Require Export Base.
Require Export Env.


(* Types ************************************************************)
Inductive ty  : Type :=
 | TCon  : nat -> ty
 | TFun  : ty  -> ty -> ty.
Hint Constructors ty.


(* Type Environments *)
Definition tyenv := env ty.


(* Expressions ******************************************************
   We use deBruijn indices for binders.
 *)
Inductive exp : Type :=
 | XVar  : nat -> exp
 | XLam  : ty  -> exp -> exp
 | XApp  : exp -> exp -> exp.
 Hint Constructors exp.


(* Weak Head Normal Forms cannot be reduced further by 
   call-by-value evaluation.
 *)
Inductive whnfX : exp -> Prop :=
 | Whnf_XVar 
   : forall i
   , whnfX (XVar i)

 | Whnf_XLam
   : forall t1 x2
   , whnfX (XLam t1 x2).
Hint Constructors whnfX.


(* Well formed expressions are closed under the given environment. *)
Fixpoint wfX (tenv: tyenv) (xx: exp) : Prop :=
 match xx with  
 | XVar i     => exists t, get tenv i = Some t
 | XLam t x   => wfX (tenv :> t) x
 | XApp x1 x2 => wfX tenv x1 /\ wfX tenv x2
 end.


(* Closed expressions are well formed under an empty environment. *)
Definition closedX (xx: exp) : Prop
 := wfX Empty xx.
Hint Unfold closedX.


(* Values are closed expressions that cannot be reduced further. *)
Inductive value : exp -> Prop :=
 | Value 
   :  forall xx
   ,  whnfX xx -> closedX xx
   -> value xx.
Hint Constructors value.

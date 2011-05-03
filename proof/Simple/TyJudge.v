
Require Export Exp.


(** Type Judgements *************************************************)
Inductive TYPE : tyenv -> exp -> ty -> Prop :=
 | TYVar 
   :  forall tenv i t
   ,  get tenv i = Some t
   -> TYPE tenv (XVar i) t

 | TYLam
   :  forall tenv x t1 t2
   ,  TYPE (tenv :> t1) x t2
   -> TYPE tenv (XLam t1 x) (TFun t1 t2)

 | TYApp
   :  forall tenv x1 x2 t1 t2
   ,  TYPE tenv x1 (TFun t1 t2)
   -> TYPE tenv x2 t1
   -> TYPE tenv (XApp x1 x2) t2.

Hint Constructors TYPE.


(* Well Formedness **************************************************)
(* A well typed expression is well formed *)
Theorem type_wfX
 :  forall tenv x t
 ,  TYPE tenv x t
 -> wfX  tenv x.
Proof.
 intros. gen tenv t.
 induction x; intros; inverts H; simpl; eauto.
Qed.
Hint Resolve type_wfX.
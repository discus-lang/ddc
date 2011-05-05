
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


(* Weakening Type Env in Type Judgement *****************************
   We can insert a new type into the type environment, provided we
   lift existing references to types higher in the stack across
   the new one.
 *)
Lemma type_tyenv_insert
 :  forall e ix x t1 t2
 ,  TYPE e x t1
 -> TYPE (insert ix t2 e) (liftX ix x) t1.
Proof.
 intros. gen ix e t1.
 induction x; intros; simpl; inverts H; eauto.

 Case "XVar".
  lift_cases; intros; auto.

 Case "XLam".
  apply TYLam.
  rewrite insert_rewind. 
   apply IHx. auto.
Qed.


Lemma type_tyenv_weaken
 :  forall e x t1 t2
 ,  TYPE  e         x            t1
 -> TYPE (e :> t2) (liftX 0 x) t1.
Proof.
 intros.
 assert (e :> t2 = insert 0 t2 e).
  simpl. destruct e; auto.
  rewrite H0. apply type_tyenv_insert. auto.
Qed.



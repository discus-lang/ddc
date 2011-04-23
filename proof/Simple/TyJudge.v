
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


(* Weakening type environments. *************************************)
Theorem type_tyenv_weaken1
 :  forall tenv x t1 t2
 ,  TYPE tenv          x t1
 -> TYPE (t2 <: tenv)  x t1.
Proof.
 intros. gen tenv t1.
 induction x; intros; inversions H; eauto.

 Case "XLam".
  eapply TYLam. rewrite snoc_cons. auto.
Qed.


Theorem type_tyenv_weaken
 :  forall tenv1 tenv2 x1 t1
 ,  TYPE tenv1            x1 t1
 -> TYPE (tenv2 ++ tenv1) x1 t1.
Proof.
 intros. gen tenv1.
 induction tenv2; intros.
  rewrite append_empty_left. auto.
  rewrite append_snoc.  apply IHtenv2.
   apply type_tyenv_weaken1. auto.
Qed.


(* Strengthen type environments *************************************)
Theorem type_tyenv_strengthen
 :  forall tenv tenv' n x t
 ,   wfX tenv' x
 ->  tenv' = take n tenv
 ->  TYPE tenv  x t
 ->  TYPE tenv' x t.
Proof.
 intros. gen tenv tenv' n t.
 induction x; intros; inversions H1.
 
 Case "XVar".
  apply TYVar.
   destruct H. eauto. 

 Case "XLam".
  simpl in H.
  eapply TYLam.
   eapply IHx with (n := S n) (tenv := tenv :> t); auto.

 Case "XApp".
  simpl in H. destruct H. eauto.
Qed.


(* Checking closed expressions **************************************)
Theorem type_check_empty_tyenv_is_closed
 :  forall x t
 ,  TYPE Empty x t
 -> closedX x.
Proof.
 intros. unfold closedX. eapply type_wfX. eauto.
Qed.


Theorem type_check_closed_in_empty_tyenv
 :  forall tenv x t
 ,  closedX x
 -> TYPE tenv  x t
 -> TYPE Empty x t.
Proof.
 intros. unfold closedX in H. 
 eapply type_tyenv_strengthen; eauto.
 eapply take_zero.
Qed.


Theorem type_check_closed_in_any_tyenv
 :  forall tenv tenv' x1 t1
 ,  closedX x1
 -> TYPE tenv  x1 t1
 -> TYPE tenv' x1 t1.
Proof.
 intros.
 lets D: type_check_closed_in_empty_tyenv H H0.
 assert (TYPE (tenv' ++ Empty) x1 t1).
  apply type_tyenv_weaken. auto.
  auto.
Qed.


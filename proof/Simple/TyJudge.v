
Require Export Exp.


(** Type Judgements *************************************************)
Inductive TYPE : tyenv -> exp -> ty -> Prop :=
 | TYVar 
   :  forall tenv i T
   ,  get tenv i = Some T
   -> TYPE tenv (XVar i) T  

 | TYLam
   :  forall tenv t T1 T2
   ,  TYPE (tenv :> T1) t T2
   -> TYPE tenv (XLam T1 t) (TFun T1 T2)

 | TYApp
   :  forall tenv t1 t2 T1 T2
   ,  TYPE tenv t1 (TFun T1 T2)
   -> TYPE tenv t2 T1
   -> TYPE tenv (XApp t1 t2) T2.

Hint Constructors TYPE.


(* Well Formedness **************************************************)
(* A well typed expression is well formed *)
Lemma type_wfX
 :  forall tenv x t
 ,  TYPE tenv x t
 -> wfX  tenv x.
Proof.
 intros. gen tenv t.
 induction x; intros; inverts H; simpl; eauto.
Qed.


(* Weakening type environments. *************************************)
Theorem type_tyenv_weaken1
 :  forall tenv t T1 T2
 ,  TYPE tenv          t T1
 -> TYPE (T2 <: tenv)  t T1.
Proof.
 intros. gen tenv T1.
 induction t; intros; inversions H; eauto.

 Case "XLam".
  eapply TYLam. rewrite snoc_cons. auto.
Qed.


Theorem type_tyenv_weaken
 :  forall tenv1 tenv2 t1 T1
 ,  TYPE tenv1            t1 T1
 -> TYPE (tenv2 ++ tenv1) t1 T1.
Proof.
 intros. gen tenv1.
 induction tenv2; intros.
  rewrite append_empty_left. auto.
  rewrite append_snoc.  apply IHtenv2.
   apply type_tyenv_weaken1. auto.
Qed.


(* Strengthen type environments *************************************)
Theorem type_tyenv_strengthen
 :  forall tenv tenv' n t T
 ,   wfX tenv' t
 ->  tenv' = take n tenv
 ->  TYPE tenv  t T
 ->  TYPE tenv' t T.
Proof.
 intros. gen tenv tenv' n T.
 induction t; intros; inversions H1.
 
 Case "XVar".
  apply TYVar.
   destruct H. eauto. 

 Case "XLam".
  simpl in H.
  eapply TYLam.
   eapply IHt with (n := S n) (tenv := tenv :> t); auto.

 Case "XApp".
  simpl in H. destruct H.
  eapply TYApp.
   eapply IHt1; eauto.
   eapply IHt2; eauto.
Qed.


(* Checking closed expressions ******************)
Lemma type_check_empty_tyenv_is_closed
 :  forall t T
 ,  TYPE Empty t T
 -> closedX t.
Proof.
 intros. unfold closedX. eapply type_wfX. eauto.
Qed.


Theorem type_check_closed_in_empty_tyenv
 :  forall tenv t T
 ,  closedX t
 -> TYPE tenv  t T
 -> TYPE Empty t T.
Proof.
 intros. unfold closedX in H. 
 eapply type_tyenv_strengthen; eauto.
 eapply take_zero.
Qed.


Theorem type_check_closed_in_any_tyenv
 :  forall tenv tenv' t1 T1
 ,  closedX t1
 -> TYPE tenv  t1 T1
 -> TYPE tenv' t1 T1.
Proof.
 intros.
 lets D: type_check_closed_in_empty_tyenv H H0.
 assert (TYPE (tenv' ++ Empty) t1 T1).
  apply type_tyenv_weaken. auto.
  auto.
Qed.


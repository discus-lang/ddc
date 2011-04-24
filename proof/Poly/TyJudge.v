
Require Import Exp.
Require Import Ty.
Require Import Env.
Require Import KiJudge.
Require Import SubstTypeType.
Require Import Base.

(* Check well typeness of terms. *)
Inductive TYPE : kienv -> tyenv -> exp -> ty -> Prop :=
 | TYVar 
   :  forall kenv tenv ix T
   ,  wfEnv kenv tenv
   -> get tenv ix = Some T
   -> TYPE kenv tenv (XVar ix) T

 | TYLam 
   :  forall kenv tenv x12 t11 t12
   ,  TYPE kenv (tenv :> t11) x12            t12
   -> TYPE kenv tenv          (XLam t11 x12) (TFun t11 t12)

 | TYApp 
   :  forall kenv tenv x1 x2 t11 t12
   ,  TYPE kenv tenv x1 (TFun t11 t12) 
   -> TYPE kenv tenv x2 t11
   -> TYPE kenv tenv (XApp x1 x2) t12

 | TYLAM
   :  forall kenv tenv x1 t1
   ,  TYPE (kenv :> KStar) tenv x1        t1
   -> TYPE kenv            tenv (XLAM x1) (TForall t1)

 | TYAPP
   :  forall kenv tenv x1 t1 t2
   ,  TYPE kenv tenv x1 (TForall t1)
   -> KIND kenv t2 KStar
   -> TYPE kenv tenv (XAPP x1 t2) (substTT t2 t1). 

Hint Constructors TYPE.


(* Well Formedness **************************************************)

(* The environment used to type an expression is well formed *)
Theorem type_wfEnv
 :  forall kenv tenv x t
 ,  TYPE kenv tenv x t
 -> wfEnv kenv tenv.
Proof.
 intros. gen kenv tenv t.
 induction x; intros.

 Case "XVar".
  inverts H. auto.

 Case "XLAM".
  inverts H. apply IHx in H3. admit.

 Case "XAPP".
  inverts H. eauto.

 Case "XLam".
  inverts H. apply IHx in H5. simpl in H5. tauto.

 Case "XApp".
  inverts H. eauto. 
Qed.


(* A well typed expression is well formed *)
Theorem type_wfX
 :  forall kenv tenv x t
 ,  TYPE kenv tenv x t
 -> wfX  kenv tenv x /\ wfT kenv t.
Proof.
 intros. gen kenv tenv t.
 induction x; intros; simpl; eauto.

 Case "XVar".
  inverts H. split. eauto.
  eapply wfT_from_wfEnv; eauto.

 Case "XLAM".
  inverts H. apply IHx in H3. eauto.

 Case "XAPP".
  inverts H. apply IHx in H4. destruct H4.
   split. split. auto.
   eapply kind_wfT. eauto.
   simpl in H0.
   admit. (* subst types still well formed *)

 Case "XLam".
  inverts H. 
  lets D: IHx H5. destruct D.
  split. split.
   admit. 
   auto.
   simpl. split.
    admit.
    auto.

 Case "XApp".
  inverts H.
  apply IHx1 in H4. destruct H4.
  apply IHx2 in H6. destruct H6.
  simpl in H0. tauto.
Qed. 


(* Weakening type environment ***************************************)
Lemma type_tyenv_weaken1
 :  forall kenv tenv x1 t1 t2
 ,  TYPE kenv tenv         x1 t1
 -> TYPE kenv (t2 <: tenv) x1 t1.
Proof.
 intros. gen kenv tenv t1.
 induction x1; intros; inverts H; eauto.

 Case "XLam".
  apply TYLam. rewrite snoc_cons. auto.
Qed.


Lemma type_tyenv_weaken
 :  forall kenv tenv1 tenv2 x1 t1
 ,  TYPE kenv tenv1            x1 t1
 -> TYPE kenv (tenv2 ++ tenv1) x1 t1.
Proof.
 intros. gen kenv tenv1 t1.
 induction tenv2; intros.
  rewrite append_empty_left. auto.
  rewrite append_snoc. apply IHtenv2.
   apply type_tyenv_weaken1. auto.
Qed.


(* Weakening kind environment ***************************************)
Lemma type_kienv_weaken1
 :  forall kenv tenv x1 t1 k1
 ,  TYPE kenv tenv         x1 t1
 -> TYPE (k1 <: kenv) tenv x1 t1.
Proof.
 intros. gen kenv tenv t1.
 induction x1; intros; inverts H; eauto.

 Case "XLAM".
  apply TYLAM. rewrite snoc_cons. auto.

 Case "XAPP".
  apply TYAPP; auto.
  apply kind_kienv_weaken1. auto.
Qed.


Lemma type_kienv_weaken
 :  forall kenv1 kenv2 tenv x t
 ,  TYPE kenv1            tenv x t
 -> TYPE (kenv2 ++ kenv1) tenv x t.
Proof.
 intros. gen kenv1 tenv t.
 induction kenv2; intros.
  rewrite append_empty_left. auto.
  rewrite append_snoc. apply IHkenv2.
   apply type_kienv_weaken1. auto.
Qed.


(* Strenghten type environment **************************************)
Lemma type_tyenv_strengthen
 :  forall kenv tenv tenv' n x t
 ,   coversXX n x
 ->  tenv' = take n tenv
 ->  TYPE kenv tenv  x t
 ->  TYPE kenv tenv' x t.
Proof.
 intros. gen kenv tenv tenv' n t.
 induction x; intros; inverts H1; inverts H; eauto.

 Case "XVar".
  apply TYVar. subst.
  apply get_take; auto.

 Case "XLam".
  apply TYLam. subst.
  eapply IHx with (n := S n) (tenv := tenv :> t); auto.
Qed.


(* Strenghten kind environment **************************************)
Lemma type_kienv_strengthen
 :  forall kenv kenv' tenv n x t
 ,   coversTX n x
 ->  kenv' = take n kenv
 ->  TYPE kenv  tenv x t
 ->  TYPE kenv' tenv x t.
Proof.
 intros. gen kenv kenv' tenv n t.
 induction x; intros; inverts H1; inverts H; eauto.

 Case "XLAM".
  apply TYLAM. subst.
  apply IHx with (n := S n) (kenv := kenv :> KStar); auto.

 Case "XAPP".
  apply TYAPP. eauto. subst.
  eapply kind_kienv_strengthen; auto. auto.
Qed.


(* Checking closed expressions **************************************)

(* Closure under type environment ***************)
Lemma type_check_closedUnderXX
 :  forall kenv tenv x t
 ,  TYPE   kenv tenv x t
 -> closedUnderXX tenv x.
Proof.
 intros. eapply ClosedUnderXX. gen kenv tenv t.
 induction x; intros; inverts H; eauto.

 Case "XLam".
  apply IHx in H5. auto.
Qed.


Lemma type_check_empty_tyenv_is_tyclosed
 :  forall kenv     x t
 ,  TYPE kenv Empty x t 
 -> closedXX x.
Proof.
 intros. eapply type_check_closedUnderXX in H.
 inverts H. auto.
Qed.


Lemma type_check_tyclosed_in_empty_tyenv
 :  forall kenv tenv x t
 ,  closedXX x 
 -> TYPE kenv tenv  x t
 -> TYPE kenv Empty x t.
Proof.
 intros. inverts H.
 eapply type_tyenv_strengthen; eauto.
Qed.


Lemma type_check_tyclosed_in_any_tyenv
 :  forall kenv tenv tenv' x t
 ,  closedXX x
 -> TYPE kenv tenv  x t
 -> TYPE kenv tenv' x t.
Proof.
 intros.
 lets D: type_check_tyclosed_in_empty_tyenv H H0.
 assert (TYPE kenv (tenv' ++ Empty) x t).
  apply type_tyenv_weaken. auto.
  auto.
Qed.


(* Closure under kind environment ***************)
Lemma type_check_closedUnderTX
 :  forall kenv tenv x t
 ,  TYPE kenv tenv x t
 -> closedUnderTX kenv x.
Proof.
 intros. eapply ClosedUnderTX. gen kenv tenv t.
 induction x; intros; inverts H; eauto.

 Case "XLAM".
  apply IHx in H3. auto.

 Case "XAPP".
  apply CoversTX_APP. eapply IHx; eauto.
  apply kind_check_closedUnderTT in H6. inverts H6. auto.
Qed.


Lemma type_check_empty_kienv_is_kiclosed
 :  forall tenv x t
 ,  TYPE Empty tenv x t
 -> closedTX x.
Proof.
 intros. eapply type_check_closedUnderTX in H.
 inverts H. auto.
Qed.


Lemma type_check_kiclosed_in_empty_kienv
 :  forall kenv tenv x t
 ,  closedTX x
 -> TYPE kenv  tenv x t
 -> TYPE Empty tenv x t.
Proof.
 intros. inverts H.
 eapply type_kienv_strengthen; eauto.
Qed.


Lemma type_check_kiclosed_in_any_kienv
 :  forall kenv kenv' tenv x t
 ,  closedTX x
 -> TYPE kenv  tenv x t
 -> TYPE kenv' tenv x t.
Proof.
 intros.
 lets D: type_check_kiclosed_in_empty_kienv H H0.
 assert (TYPE (kenv' ++ Empty) tenv x t).
  apply type_kienv_weaken. auto.
  auto.
Qed.


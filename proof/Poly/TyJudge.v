
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
   ,  get tenv ix = Some T
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
   -> TYPE kenv tenv (XAPP x1 t2) (substT t2 t1). 

Hint Constructors TYPE.


(* Weakening type environment ***************************************)
Lemma type_tyenv_weaken1
 :  forall kenv tenv x1 t1 t2
 ,  TYPE kenv tenv         x1 t1
 -> TYPE kenv (t2 <: tenv) x1 t1.
Proof.
 intros. gen kenv tenv t1.
 induction x1; intros; inversions H; eauto.

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
 induction x1; intros; inversions H; eauto.

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
 induction x; intros; inversions H1; inversions H; eauto.

 Case "XVar".
  apply TYVar.
  apply get_take; auto.

 Case "XLam".
  apply TYLam.
  eapply IHx with (n := S n) (tenv := tenv :> t); auto.
Qed.


(* Strenghten kind environment **************************************)
Lemma type_kienv_strengthen
 :  forall kenv kenv' tenv n x t
 ,   coversXT n x
 ->  kenv' = take n kenv
 ->  TYPE kenv  tenv x t
 ->  TYPE kenv' tenv x t.
Proof.
 intros. gen kenv kenv' tenv n t.
 induction x; intros; inversions H1; inversions H; eauto.

 Case "XLAM".
  apply TYLAM.
  apply IHx with (n := S n) (kenv := kenv :> KStar); auto.

 Case "XAPP".
  apply TYAPP. eauto.
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
 induction x; intros; inversions H; eauto.

 Case "XLam".
  apply IHx in H5. auto.
Qed.


Lemma type_check_empty_tyenv_is_tyclosed
 :  forall kenv     x t
 ,  TYPE kenv Empty x t 
 -> closedXX x.
Proof.
 intros. eapply type_check_closedUnderXX in H.
 inversions H. auto.
Qed.


Lemma type_check_tyclosed_in_empty_tyenv
 :  forall kenv tenv x t
 ,  closedXX x 
 -> TYPE kenv tenv  x t
 -> TYPE kenv Empty x t.
Proof.
 intros. inversions H.
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
Lemma type_check_closedUnderXT
 :  forall kenv tenv x t
 ,  TYPE kenv tenv x t
 -> closedUnderXT kenv x.
Proof.
 intros. eapply ClosedUnderXT. gen kenv tenv t.
 induction x; intros; inversions H; eauto.

 Case "XLAM".
  apply IHx in H3. auto.

 Case "XAPP".
  apply CoversXT_APP. eapply IHx; eauto.
  apply kind_check_closedUnderT in H6. inversions H6. auto.
Qed.


Lemma type_check_empty_kienv_is_kiclosed
 :  forall tenv x t
 ,  TYPE Empty tenv x t
 -> closedXT x.
Proof.
 intros. eapply type_check_closedUnderXT in H.
 inversions H. auto.
Qed.


Lemma type_check_kiclosed_in_empty_kienv
 :  forall kenv tenv x t
 ,  closedXT x
 -> TYPE kenv  tenv x t
 -> TYPE Empty tenv x t.
Proof.
 intros. inversions H.
 eapply type_kienv_strengthen; eauto.
Qed.


Lemma type_check_kiclosed_in_any_kienv
 :  forall kenv kenv' tenv x t
 ,  closedXT x
 -> TYPE kenv  tenv x t
 -> TYPE kenv' tenv x t.
Proof.
 intros.
 lets D: type_check_kiclosed_in_empty_kienv H H0.
 assert (TYPE (kenv' ++ Empty) tenv x t).
  apply type_kienv_weaken. auto.
  auto.
Qed.


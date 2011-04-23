
Require Export Ty.
Require Export Env.
Require Import Base.

(* Kinds of types ***************************************************)
Inductive KIND : kienv -> ty -> ki -> Prop :=
 | KICon 
   :  forall kenv c
   ,  KIND kenv (TCon c) KStar

 | KIVar
   :  forall kenv it k
   ,  get kenv it = Some k
   -> KIND kenv (TVar it) k

 | KIForall
   :  forall kenv t
   ,  KIND (kenv :> KStar) t           KStar
   -> KIND kenv            (TForall t) KStar

 | KIFun 
   :  forall kenv t1 t2
   ,  KIND kenv t1 KStar -> KIND kenv t2 KStar
   -> KIND kenv (TFun t1 t2) KStar.
Hint Constructors KIND.


(* Well Formedness **************************************************)

(* A well kinded type is well formed *)
Theorem kind_wfT
 :  forall kenv t k
 ,  KIND kenv t k
 -> wfT kenv t.
Proof.
 intros. gen kenv k.
 induction t; intros; inverts H; simpl; eauto.
Qed.


(* Weakening kind environment ***************************************)
Lemma kind_kienv_weaken1
 :  forall kenv t1 k1 k2
 ,  KIND kenv         t1 k1
 -> KIND (k2 <: kenv) t1 k1.
Proof.
 intros. gen kenv k1.
 induction t1; intros; inverts H; eauto.

 Case "TForall".
  apply KIForall.
  rewrite snoc_cons. apply IHt1. auto.
Qed.


Lemma kind_kienv_weaken
 :  forall kenv1 kenv2 t1 k1
 ,  KIND kenv1            t1 k1
 -> KIND (kenv2 ++ kenv1) t1 k1.
Proof.
 intros. gen kenv1 k1.
 induction kenv2; intros.
  rewrite append_empty_left. auto.
  rewrite append_snoc. apply IHkenv2.
   apply kind_kienv_weaken1. auto.
Qed.


(* Strenghten kind environment **************************************)
Lemma kind_kienv_strengthen
 :  forall kenv kenv' n t k
 ,   wfT kenv' t
 ->  kenv' = take n kenv
 ->  KIND kenv  t k
 ->  KIND kenv' t k.
Proof.
 intros. gen kenv kenv' n k.
 induction t; intros; inverts H1; eauto.
 Case "TVar".
  apply KIVar.
   destruct H.  subst. eauto.

 Case "TForall".
  apply KIForall. subst.
  eapply IHt with (n := S n) (kenv := kenv :> KStar); auto.

 Case "TFun".
  inverts H. eauto.
Qed.


(* Checking closed types ********************************************)
Lemma kind_check_empty_is_closed
 :  forall     t k
 ,  KIND Empty t k 
 -> closedT t.
Proof.
 intros. unfold closedT. eapply kind_wfT. eauto.
Qed.


Lemma kind_check_closed_in_empty
 :  forall kenv t k
 ,  closedT t
 -> KIND kenv  t k
 -> KIND Empty t k.
Proof.
 intros. unfold closedT in H.
 eapply kind_kienv_strengthen; eauto.
 eapply take_zero.
Qed.


Theorem kind_check_closed_in_any
 :  forall kenv kenv' t k
 ,  closedT t
 -> KIND kenv  t k
 -> KIND kenv' t k.
Proof.
 intros.
 lets D: kind_check_closed_in_empty H H0.
 assert (KIND (kenv' ++ Empty) t k).
  apply kind_kienv_weaken. auto.
  auto.
Qed.


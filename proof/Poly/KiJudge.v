
Require Import Exp.
Require Import TyEnv.
Require Import WellFormed.
Require Import Base.

(* Kinds of types ***************************************************)

Inductive KIND : tyenv -> ty -> ki -> Prop :=
 | KICon 
   :  forall e c
   ,  KIND e (TCon c) KStar

 | KIVar
   :  forall e it k
   ,  getK e it = Some (EKind k)
   -> KIND e (TVar it) k

 | KIForall
   :  forall e t
   ,  KIND (e :> EKind KStar) t          KStar
   -> KIND e                 (TForall t) KStar

 | KIFun 
   :  forall e t1 t2
   ,  KIND e t1 KStar -> KIND e t2 KStar
   -> KIND e (TFun t1 t2) KStar.
Hint Constructors KIND.


(* Well Formedness **************************************************)

(* A well kinded type is well formed *)
Lemma kind_wfT
 :  forall e t k
 ,  KIND e t k
 -> wfT  e t.
Proof.
 intros. gen e k.
 induction t; intros; inverts H; simpl; eauto.
Qed.


(* Weakening kind environment ***************************************)

(* If we add a new kind to the environment of a well kinded type, 
   then that type is still well kinded *)
Lemma kind_weaken1
 :  forall e t1 k1 l
 ,  KIND e        t1 k1
 -> KIND (l <: e) t1 k1.
Proof.
 intros. gen e k1.
 induction t1; intros; inverts H; eauto.

 Case "TVar".
  apply KIVar. 
  apply getMatch_cons_some. auto.

 Case "TForall".
  apply KIForall.
  rewrite snoc_cons. apply IHt1. auto.
Qed.


Lemma kind_weaken
 :  forall e1 e2 t1 k1
 ,  KIND e1         t1 k1
 -> KIND (e2 ++ e1) t1 k1.
Proof.
 intros. gen e1 k1.
 induction e2; intros.
  rewrite append_empty_left. auto.
  rewrite append_snoc. apply IHe2.
   apply kind_weaken1. auto.
Qed.


(* Strenghten kind environment **************************************)

(* If a well kinded type doesn't use elements on the end of its
   environment, then it is still well kinded if we cut them off *)
Lemma kind_strengthen
 :  forall e e' n t k
 ,   wfT e' t
 ->  e' = take n e
 ->  KIND e  t k
 ->  KIND e' t k.
Proof.
 intros. gen e e' n k.
 induction t; intros; inverts H1; eauto.
 Case "TVar".
  apply KIVar. subst.
  destruct H. admit. (* ok, list lemma *)

 Case "TForall".
  apply KIForall. subst.
  eapply IHt with (n := S n) (e := e :> EKind KStar); auto.

 Case "TFun".
  simpl in H. destruct H. eauto. 
Qed.


(* Checking closed types ********************************************)

(* If a type is well kinded in an empty environment,
   then that type is closed. *)
Lemma kind_empty_is_closed
 :  forall t k
 ,  KIND Empty t k 
 -> closedT t.
Proof.
 intros. unfold closedT. eapply kind_wfT. eauto.
Qed.


(* If a closed type is well kinded,
   then it is well kinded in an empty environment. *)
Lemma kind_closed_in_empty
 :  forall e t k
 ,  closedT t
 -> KIND e     t k
 -> KIND Empty t k.
Proof.
 intros. unfold closedT in H.
 eapply kind_strengthen; eauto.
 eapply take_zero.
Qed.


(* If a closed type is well kinded,
   then it is well kinded in an any environment. *)
Theorem kind_closed_in_any
 :  forall e e' t k
 ,  closedT t
 -> KIND e  t k
 -> KIND e' t k.
Proof.
 intros.
 lets D: kind_closed_in_empty H H0.
 assert (KIND (e' ++ Empty) t k).
  apply kind_weaken. auto.
  auto.
Qed.



Require Import TyJudge.
Require Import EvJudge.
Require Import Exp.
Require Import Substitute.
Require Import Base.
Require Import Coq.Arith.EqNat.

Theorem type_tyenv_strengthen
 :  forall tenv tenv' n t T
 ,   coversX n t
 ->  tenv' = take n tenv
 ->  TYPE tenv  t T
 ->  TYPE tenv' t T.
Proof.
 intros. gen tenv tenv' n T.
 induction t; intros.
 
 inversions H. inversions H1.
 apply TYVar. admit. (* ok, n0 > n *)

 inversions H1. eapply TYLam.
 eapply IHt. inversions H. apply H2.
 assert (take n tenv :> t = take (S n) (tenv :> t)). simpl. auto.
 apply H0. auto.

 inversions H1. 
 eapply TYApp.
 eapply IHt1. inversions H. eauto. eauto. eauto.
 eapply IHt2. inversions H. eauto. eauto. eauto.
Qed.


Theorem type_tyenv_weaken
 :  forall tenv t T1 T2
 ,  TYPE tenv          t T1
 -> TYPE (T2 <: tenv)  t T1.
Proof.
 intros. gen tenv T1.
 induction t; intros.

 inversions H. eapply TYVar. admit. (* admit ok *)

 inversions H. eapply TYLam. rewrite env_snoc_cons.
 apply IHt. auto.

 inversions H. eapply TYApp; eauto.
Qed.


Theorem type_check_closed_in_empty
 :  forall tenv t T
 ,  closedX t
 -> TYPE tenv  t T
 -> TYPE empty t T.
Proof.
 intros. inversions H.
 eapply type_tyenv_strengthen. eauto. eauto. eauto.
Qed.


Theorem type_tyenv_invariance
 : forall tenv tenv' t T
 ,  TYPE tenv  t T
 -> (forall n m, coversX m t -> n <= m -> get tenv' n = get tenv n)
 -> TYPE tenv' t T.
Proof.
 intros. gen tenv'. induction H; intros.
 
 eapply TYVar. rewrite <- H. eapply H0 with (S i). eauto. eauto.

 eapply TYLam.
  eapply IHTYPE. intros.
   destruct n. simpl. auto.
   simpl. eapply H0 with (S m).
   eapply CoversX_lam.
   eapply coversX_weaken_succ.
   eapply coversX_weaken_succ. auto.
   omega.
  
 eapply TYApp.
  eapply IHTYPE1. intros. eapply H1. skip. auto. (* ok, big enough *)
  eapply IHTYPE2. intros. eapply H1. skip. auto. (* ok, big enough *)
Qed.

 

Theorem subst_value_value
 :  forall ix tenv t1 t2 T1 T2
 ,  get tenv ix = some T2
 -> closedX t2
 -> TYPE tenv           t1 T1
 -> TYPE (drop ix tenv) t2 T2
 -> TYPE (drop ix tenv) (subLocalX' ix t2 t1) T1.
Proof.
 intros ix tenv t1 t2 T1 T2.
 gen ix tenv T1.
 induction t1; intros.

 Case "XVar".
  unfold subLocalX'.
  remember (compare n ix) as e. destruct e.
  SCase "n = ix".
   apply compare_eq in Heqe. subst.
   rewrite liftX_closed.
   inversions H1. rewrite H in H5. inversions H5. auto. auto.

  SCase "n < ix".
   apply TYVar. inversions H1.
   rewrite <- H5. admit (* ok, n < ix *).

  SCase "n > ix".
   apply TYVar. inversions H1.
   rewrite <- H5.
   admit. (* probably ok *)

 Case "XLam".
  inversions H1.
  simpl. 
  apply TYLam.
  rewrite drop_rewind.
  apply IHt1. 
   simpl. auto. auto.
   auto.
   simpl. 
   lets D: type_check_closed_in_empty H0 H2.
   eapply type_tyenv_invariance. eauto.
   intros. rewrite drop_rewind.
   

admit. (* ok, t2 is closed *)
  
 Case "XApp".
  inversions H1.
  simpl.
  eapply TYApp.  
   apply IHt1_1; eauto.
   apply IHt1_2; eauto.
Qed.


Theorem subst_value_value_push
 :  forall tenv t1 t2 T1 T2
 ,  closedX t2
 -> TYPE (tenv :> T2) t1 T1
 -> TYPE tenv         t2 T2 
 -> TYPE tenv (subLocalX t2 t1) T1.
Proof. 
 intros tenv t1 t2 T1 T2 Ht1 Ht2.
 lets H: subst_value_value 0 (tenv :> T2).
  simpl in H.
  eapply H; eauto.
Qed.


(* When a well typed term transitions to the next state, 
   its type is perserved. *)
Theorem preservation
 :  forall t t' T
 ,  TYPE empty t  T
 -> STEP t t'
 -> TYPE empty t' T.
Proof.
 intros t t' T HT HS. gen T t'.
 induction t; intros.

 Case "XVar".
  inversion HS.

 Case "XLam".
  inversion HS.

 Case "XApp".
  inversions HT.
  inverts keep HS.

  SCase "EVLamApp".
   inversions H2.
   inversions H3.
   eapply subst_value_value_push; eauto.

  SCase "EVApp1".
   eapply IHt1 in H2; eauto.
   eapply IHt2 in H4; eauto.
Qed.


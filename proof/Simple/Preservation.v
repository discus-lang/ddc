
Require Import TyJudge.
Require Import EvJudge.
Require Import Exp.
Require Import Substitute.
Require Import Base.
Require Import Coq.Arith.EqNat.


(* We can type an expression with an environment one elem larger *)
Theorem type_tyenv_weaken1
 :  forall tenv t T1 T2
 ,  TYPE tenv          t T1
 -> TYPE (T2 <: tenv)  t T1.
Proof.
 intros. gen tenv T1.
 induction t; intros; inversions H.

 Case "XVar".
  eapply TYVar. apply get_weaken1. auto.

 Case "XLam".
  eapply TYLam. rewrite env_snoc_cons.
  apply IHt. auto.

 Case "XApp".
  eapply TYApp; eauto.
Qed.


Theorem type_tyenv_weaken
 :  forall tenv1 tenv2 t1 T1
 ,  TYPE tenv1            t1 T1
 -> TYPE (tenv2 ++ tenv1) t1 T1.
Proof.
 intros. gen tenv1.
 induction tenv2; intros.
  rewrite append_empty. auto.
  rewrite append_snoc.  apply IHtenv2.
   apply type_tyenv_weaken1. auto.
Qed.


Theorem type_tyenv_strengthen
 :  forall tenv tenv' n t T
 ,   coversX n t
 ->  tenv' = take n tenv
 ->  TYPE tenv  t T
 ->  TYPE tenv' t T.
Proof.
 intros. gen tenv tenv' n T.
 induction t; intros; inversions H1.
 
 Case "XVar".
  apply TYVar. inversions H.
  apply get_take. auto. auto.

 Case "XLam".
  eapply TYLam. inversions H.
  eapply IHt.  apply H2.
  assert (take n tenv :> t = take (S n) (tenv :> t)). simpl. auto.
  eauto. auto.

 Case "XApp".
  inversions H.
  eapply TYApp.
  eapply IHt1; eauto.
  eapply IHt2; eauto.
Qed.


Theorem type_check_closed_in_empty
 :  forall tenv t T
 ,  closedX t
 -> TYPE tenv  t T
 -> TYPE empty t T.
Proof.
 intros. inversions H.
 eapply type_tyenv_strengthen; eauto.
Qed.


Theorem type_check_closed_in_any
 :  forall tenv tenv' t1 T1
 ,  closedX t1
 -> TYPE tenv  t1 T1
 -> TYPE tenv' t1 T1.
Proof.
 intros.
 lets D: type_check_closed_in_empty H H0. clear H0.
 assert (TYPE (tenv' ++ empty) t1 T1).
  apply type_tyenv_weaken. auto.
  simpl in H0. auto.
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
   apply compare_lt in Heqe.
    rewrite <- H5. eapply get_drop_above. auto.
   
  SCase "n > ix".
   apply compare_gt in Heqe.
   inversions H1.
   apply TYVar. 
    rewrite <- H5. 
     destruct n. 
      inversions Heqe.
      simpl. assert (n - 0 = n). omega. rewrite H1.
      apply get_drop_below. omega.

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
   eapply type_check_closed_in_any; eauto.
  
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


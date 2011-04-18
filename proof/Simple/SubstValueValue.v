
Require Import TyJudge.
Require Import Substitute.


Theorem subst_value_value_drop
 :  forall ix tenv t1 t2 T1 T2
 ,  get tenv ix = Some T2
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


Theorem subst_value_value
 :  forall tenv t1 t2 T1 T2
 ,  closedX t2
 -> TYPE (tenv :> T2) t1 T1
 -> TYPE tenv         t2 T2 
 -> TYPE tenv (subLocalX t2 t1) T1.
Proof. 
 intros tenv t1 t2 T1 T2 Ht1 Ht2.
 lets H: subst_value_value_drop 0 (tenv :> T2).
  simpl in H.
  eapply H; eauto.
Qed.

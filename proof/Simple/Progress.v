
Require Import EvJudge.
Require Import TyJudge.
Require Import Exp.
Require Import LibTactics.
Require Import Base.

Lemma type_check_closedUnderX 
 :  forall tenv t T
 ,  TYPE tenv t T -> closedUnderX tenv t.
Proof.
 intros. apply ClosedUnderX.
 gen tenv T.
 induction t.
 
 Case "XVar".
  intros. inversions H. eapply CoversX_var.
  eapply get_length_more. eauto.

 Case "XLam". 
  intros. inversions H. apply IHt in H4.
  apply CoversX_lam. simpl in H4. auto.

 Case "XApp".
  intros. inversions H. eapply CoversX_app; eauto.
Qed.


Lemma type_check_empty_is_closed
 :  forall t T
 ,  TYPE empty t T -> closedX t.
Proof.
 intros. apply type_check_closedUnderX in H.
 inversions H. simpl in H0. apply ClosedX. auto.
Qed.


Theorem progress
 :  forall t T
 ,  TYPE empty t T
 -> value t \/ (exists t', STEP t t').
Proof.
 intros.
 remember (@empty ty) as tenv.
 induction H.

 Case "XVar".
  subst. inversion H.

 Case "XLam".
  left. clear IHTYPE. subst. apply TYLam in H.
  subst. apply type_check_empty_is_closed in H.
  eauto.

 Case "XApp".
  right. 
  specializes IHTYPE1 Heqtenv.
  specializes IHTYPE2 Heqtenv.
  destruct IHTYPE1.
  SCase "value t1".
   destruct IHTYPE2.
   SSCase "value t2".
    inversions H1.
    exists (subLocalX t2 t). apply EVLamApp. eauto.
   SSCase "t2 steps".
    destruct H2 as [t2']. exists (XApp t1 t2'). eauto.
  SSCase "t1 steps".
   destruct H1 as [t1']. exists (XApp t1' t2). eauto.
Qed.


    

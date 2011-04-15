
Require Import EvJudge.
Require Import TyJudge.
Require Import Exp.
Require Import LibTactics.
Require Import Base.


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
  left. admit.

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


    

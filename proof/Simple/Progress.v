
Require Import EvJudge.
Require Import TyJudge.
Require Import Exp.
Require Import Base.


Theorem progress
 :  forall t T
 ,  TYPE Empty t T
 -> value t \/ (exists t', STEP t t').
Proof.
 intros.
 remember (@Empty ty) as tenv.
 induction H.

 Case "XVar".
  subst. inverts H.

 Case "XLam".
  left. clear IHTYPE. subst. apply TYLam in H.
  apply type_check_empty_tyenv_is_closed in H.
  eauto.

 Case "XApp".
  right. 
  specializes IHTYPE1 Heqtenv.
  specializes IHTYPE2 Heqtenv.
  destruct IHTYPE1.
  SCase "value x1".
   destruct IHTYPE2.
   SSCase "value x2".
    inverts H1.
    exists (subst x2 x). apply EVLamApp. auto.
   SSCase "x2 steps".
    destruct H2 as [x2']. exists (XApp x1 x2'). auto.
  SSCase "x1 steps".
   destruct H1 as [x1']. exists (XApp x1' x2). auto.
Qed.


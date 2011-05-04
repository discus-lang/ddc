
Require Import SubstExpExp.
Require Import SubstTypeExp.
Require Import SubstTypeType.
Require Import EsJudge.
Require Import TyJudge.


Theorem progress
 :  forall x
 ,  (exists t, TYPE Empty Empty x t)
 -> value x \/ (exists x', STEP x x').
Proof.
 intros.
 induction x; destruct H as [tx].

 Case "XVar".
  inverts H. false.

 Case "XLAM".
  left. apply type_wfX in H. auto.

 Case "XAPP".
  inverts H.
  destruct IHx. eauto.
  SCase "x value".
   right. inverts H. inverts H4.
    inverts H1. false.
    inverts H0.
    exists (substTX 0 t x1). eapply ESLAMAPP.
    inverts H0.
  SCase "x steps".
   right.
    destruct H as [x'].
    exists (XAPP x' t).
    apply ESAPP1. auto.

 Case "XLam".
  left. apply type_wfX in H. auto.

 Case "XApp".
  right.
  inverts H.
  destruct IHx1; eauto.
  SCase "x1 value".
   destruct IHx2; eauto.
   inverts H. 

   SSCase "x2 value".
    inverts H1. 
     inverts H2. false.
     inverts H4.
     exists (substXX 0 x2 x0). apply ESLamApp. auto.

   SSCase "x2 steps".
    destruct H0 as [x2'].
    exists (XApp x1 x2').
    apply ESApp2; auto.

  SCase "x1 steps".
   destruct H as [x1'].
   exists (XApp x1' x2).
   apply ESApp1; auto.
Qed.   



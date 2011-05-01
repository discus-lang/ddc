
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
  left. apply Value_LAM.
  apply type_wfX in H. auto.

 Case "XAPP".
  inverts H.
  destruct IHx. eauto.
  SCase "x value".
   right. inverts H.
    inverts H4.
    exists (substTX 0 t x0). 
    eapply ESLAMAPP.
  SCase "x steps".
   right.
    destruct H as [x'].
    exists (XAPP x' t).
    apply ESAPP1. auto.

 Case "XLam".
  left. apply Value_lam.
  apply type_wfX in H. auto.

 Case "XApp".
  right.
  inverts H.
  destruct IHx1; eauto.
  SCase "x1 value".
   destruct IHx2; eauto.
   inverts H. 

   SSCase "x2 value".
    exists (substXX 0 x2 x).
    apply ESLamApp. auto.
    inverts H4.

   SSCase "x2 steps".
    destruct H0 as [x2'].
    exists (XApp x1 x2').
    apply ESApp2; auto.

  SCase "x1 steps".
   destruct H as [x1'].
   exists (XApp x1' x2).
   apply ESApp1; auto.
Qed.   



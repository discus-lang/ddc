
Require Import EsJudge.
Require Import TyJudge.
Require Import Exp.
Require Import Base.


(* Progress using evaluation judgement with contexts *)
Theorem progress
 :  forall x T
 ,  TYPE Empty x T
 -> value x \/ (exists x', STEP x x').
Proof.
 intros.
 remember (@Empty ty) as tyenv.
 induction H.

 Case "XVar".
  subst. inverts H.

 Case "XLam".
  left. subst. eauto.

 Case "XApp".
  right. 
  specializes IHTYPE1 Heqtyenv.
  specializes IHTYPE2 Heqtyenv.
  destruct IHTYPE1.

  SCase "value x1".
   destruct IHTYPE2.
   SSCase "value x2".
    inverts H1. inverts H3.
     inverts H4. false.
     exists (substX 0 x2 x0).
     apply EsLamApp. auto.
   SSCase "x2 steps".
    destruct H2 as [x2'].
    exists (XApp x1 x2'). auto.

   SSCase "x1 steps".
    destruct H1 as [x1'].
    exists (XApp x1' x2).
    lets D: EsContext XcApp1. eauto.
Qed.

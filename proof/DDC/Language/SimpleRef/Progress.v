
Require Import DDC.Language.SimpleRef.Step.
Require Import DDC.Language.SimpleRef.Ty.
Require Import DDC.Language.SimpleRef.Exp.

Lemma value_ref
 :  forall x te se t
 ,  value x
 -> TYPE te se x (TRef t)
 -> (exists l, x = XLoc l).
Proof.
 intros.
 destruct x; try (inverts H0); try (inverts H; inverts H0).
 inverts H1. inverts H.
 inverts H1. eauto.
Qed.
Hint Resolve value_ref.


(* A closed, well typed expression is either a value or can 
   take a step in the evaluation. *)
Theorem progress
 :  forall se h x t
 ,  TYPEH se h
 -> TYPE  nil se x t
 -> value x \/ (exists h' x', STEP h x h' x').
Proof.
 intros se h x t HTH HT.
 remember (@nil ty) as tyenv.
 induction HT.

 Case "XVar".
  subst. inverts H.

 Case "XLoc".
  left. eauto.

 Case "XLam".
  left. subst. eauto.

 Case "XApp".
  right. 
  specializes IHHT1 Heqtyenv. auto.
  specializes IHHT2 Heqtyenv. auto.
  destruct IHHT1.

  SCase "value x1".
   destruct IHHT2.
   SSCase "value x2".
    inverts H. inverts H1.
     inverts H2. false.
     SSSCase "EsLamApp".
      exists h.
      exists (substX 0 x2 x0).
      apply EsLamApp. auto.
     SSSCase "XLoc".
      inverts HT1.

   SSCase "x2 steps".
    destruct H0 as [h'].
    destruct H0 as [x2'].
    exists h'. exists (XApp x1 x2'). 
    inverts H. auto.

   SSCase "x1 steps".
    destruct H as [h'].
    destruct H as [x1'].
    exists h'. exists (XApp x1' x2).
    lets D: EsContext XcApp1. eauto.

 Case "XNewRef".
  right.
  destruct IHHT; eauto.
  SCase "x1 steps".
   destruct H as [h'].
   destruct H as [xData'].
   exists h'. exists (XNewRef xData'). auto.

 Case "XReadRef".
  right.
  destruct IHHT; eauto.
  SCase "xRef value".
   subst. 
   assert (exists l, xRef = XLoc l). eauto.
    destruct H0 as [l]. subst.

   inverts HT.
   assert (exists xData, get l h = Some xData).
    admit. destruct H0 as [xData]. (* ok, list lemma *)

   exists h.
   exists xData. auto.

  SCase "xRef steps".
   destruct H as [h'].
   destruct H as [x'].
   exists h' (XReadRef x').
   lets D: EsContext XcReadRef. eauto.

 Case "XWriteRef".
  right.
  specializes IHHT1 Heqtyenv. auto.
  specializes IHHT2 Heqtyenv. auto.

  destruct IHHT1.
  SCase "value xRef".
   destruct IHHT2.
    SSCase "value xData".
     assert (exists l, xRef = XLoc l). eauto.
     destruct H1 as [l]. subst.
     exists (update l xData h). exists xUnit.
     auto.
    SSCase "x2 steps".
     destruct H0 as [h']. exists h'.
     destruct H0 as [xData'].
      exists (XWriteRef xRef xData'). auto.
      inverts H. auto. 
   SCase "xRef Steps".
    destruct H as [h']. 
    destruct H as [xRef']. 
     lets D: EsContext XcWriteRef1. eauto. 
Qed.

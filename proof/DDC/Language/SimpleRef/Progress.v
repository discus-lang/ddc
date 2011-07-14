
Require Import DDC.Language.SimpleRef.Step.
Require Import DDC.Language.SimpleRef.Ty.
Require Import DDC.Language.SimpleRef.Exp.



(* A closed, well typed expression is either a value or can 
   take a step in the evaluation. *)
Theorem progress
 :  forall se h x t
 ,  TYPEH se h
 -> TYPE  nil se x t
 -> value x \/ (exists h' x', STEP h x h' x').
Proof.
 intros se h x t HTH HT.
 remember (@nil ty) as te.
 induction HT; subst.

 Case "XVar".
  nope.

 Case "XLoc".
  left. eauto.

 Case "XLam".
  left. eauto.

 Case "XApp".
  right. 
  destruct IHHT1; eauto.
  SCase "value x1".
   destruct IHHT2; eauto.
   SSCase "value x2".
    SSSCase "EsLamApp".
     assert (exists t x, x1 = XLam t x) as HF. eauto.
     dests HF. subst.
     exists h (substX 0 x2 x). auto.

   SSCase "x2 steps".
    dests H0. exists h' (XApp x1 x'). eauto.

   SSCase "x1 steps".
    dests H. exists h' (XApp x' x2).
    lets D: EsContext XcApp1; eauto.

 Case "XNewRef".
  right.
  destruct IHHT; eauto.
  SCase "x1 steps".
   dests H. exists h' (XNewRef x'). auto.

 Case "XReadRef".
  right.
  destruct IHHT; eauto.
  SCase "xRef value".
   assert (exists l, xRef = XLoc l) as HF. eauto.
   dest HF. subst.
   inverts_type.
   assert (exists xData, get l h = Some xData).
    admit. dest H0. (* ok, list lemma *)
   exists h xData. auto.

  SCase "xRef steps".
   dests H.
   exists h' (XReadRef x').
   lets D: EsContext XcReadRef. eauto.

 Case "XWriteRef".
  right.
  destruct IHHT1; eauto.
  SCase "value xRef".
   destruct IHHT2; eauto.
    SSCase "value xData".
     assert (exists l, xRef = XLoc l) as HF. eauto.
     dest HF. subst.
     exists (update l xData h) xUnit. auto.
    SSCase "x2 steps".
     dests H0.
     exists h' (XWriteRef xRef x'). auto.
   SCase "xRef Steps".
    dests H. 
    lets D: EsContext XcWriteRef1. eauto.
Qed.



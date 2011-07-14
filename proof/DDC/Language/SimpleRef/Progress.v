
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


Ltac dest H
 := match goal with 
    [ H : exists a, _ |- _ ]
     => destruct H as [a]
    end.

Ltac dests H
 := repeat (dest H).


Ltac shift H
 := match goal with 
    [ H : exists a, _ |- exists b, _] 
     => destruct H as [a]; exists a
    end.

Ltac shifts H
 := repeat (shift H).


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
     assert (exists t x, x1 = XLam t x). admit.
      dests H1.
      subst.
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
    destruct HF as [l]. subst.
   inverts_type.
   assert (exists xData, get l h = Some xData).
    admit. destruct H0 as [xData]. (* ok, list lemma *)

   exists h.
   exists xData. auto.

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
      destruct HF as [l]. subst.
     exists (update l xData h). exists xUnit.
     auto.
    SSCase "x2 steps".
     dests H0.
     exists h' (XWriteRef xRef x'). auto.
   SCase "xRef Steps".
    dests H. 
    lets D: EsContext XcWriteRef1. eauto.
Qed.



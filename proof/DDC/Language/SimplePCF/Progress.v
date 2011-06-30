
Require Import DDC.Language.SimplePCF.Step.
Require Import DDC.Language.SimplePCF.Ty.
Require Import DDC.Language.SimplePCF.Exp.


(* A closed well typed expression is either a value 
   or can transition to the next state. *)
Theorem progress
 :  forall x t
 ,  TYPE nil x t
 -> value x \/ (exists x', STEP x x').
Proof.
 intros.
 remember (@nil ty) as te.
 induction H; eauto.

 Case "XVar".
  subst. inverts H.

 Case "XLam".
  left. subst. eauto. 

 Case "XApp".
  right. 
  specializes IHTYPE1 Heqte.
  specializes IHTYPE2 Heqte.
  destruct IHTYPE1.

  SCase "value x1".
   destruct IHTYPE2.
   SSCase "value x2".
    inverts H1. inverts H3.
     inverts H4. false.
     exists (substX 0 x2 x0).
     apply EsLamApp. inverts H2. auto.
     inverts H.
     inverts H.
     inverts H.
   SSCase "x2 steps".
    destruct H2 as [x2'].
    exists (XApp x1 x2'). inverts H1. auto.

   SSCase "x1 steps".
    destruct H1 as [x1'].
    exists (XApp x1' x2).
    eapply (EsContext (fun xx => XApp xx x2)); eauto.

  SCase "XSucc".
   right. 
   destruct IHTYPE. auto. subst.
    inverts H0. inverts H1;
     try inverts H2; try inverts H.
     false.
     exists (XNat (S n)).
      eapply EsSucc.
     destruct H0. exists (XSucc x). eauto.

  SCase "XPred".
   right.
   destruct IHTYPE. auto. subst.
   inverts H0. inverts H1;
    try inverts H2; try inverts H.
    false. 
    destruct n; eauto.
    destruct H0. eauto.

  SCase "XIsZero".
   right.
   destruct IHTYPE. auto. subst.
   inverts H0. inverts H1; 
    try inverts H2; try inverts H.
    false.
    destruct n; eauto.
    destruct H0. eauto.

  SCase "XIf".
   right. 
   destruct IHTYPE1. eauto.
    inverts H2; inverts H3; 
     try inverts H2; try inverts H;
     inverts H4. false.
     exists x2. eauto.
     exists x3. eauto.
     destruct H2. exists (XIf x x2 x3).
     eapply (EsContext (fun xx => XIf xx x2 x3)); eauto.
Qed.



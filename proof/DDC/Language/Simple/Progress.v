
Require Import DDC.Language.Simple.Step.
Require Import DDC.Language.Simple.Ty.
Require Import DDC.Language.Simple.Exp.


Lemma value_lam 
 :  forall xx te t1 t2
 ,  value xx 
 -> TYPE te xx (TFun t1 t2)
 -> (exists t x, xx = XLam t x).
Proof.
 intros.
 destruct xx.
 inverts H. inverts H2. inverts H.  (* want better false tactic *)
 eauto.
 inverts H. inverts H1.
Qed.
Hint Resolve value_lam.


(* A closed, well typed expression is either a value or can 
   take a step in the evaluation. *)
Theorem progress
 :  forall x t
 ,  TYPE nil x t
 -> value x \/ (exists x', STEP x x').
Proof.
 intros.
 remember (@nil ty) as tyenv.
 induction H; subst.

 Case "XVar". false.
 Case "XLam". burn.

 Case "XApp".
  right.
  assert (@nil ty = nil). auto.
   specializes IHTYPE1 H1.
   specializes IHTYPE2 H1.

  destruct IHTYPE1.
  SCase "value x1".
   destruct IHTYPE2.
   SSCase "value x2".
    assert (exists t x, x1 = XLam t x). eauto.
     destruct H4 as [t11]. 
     destruct H4 as [x11]. 
     subst.
    exists (substX 0 x2 x11). auto.

   SSCase "x2 steps".
    destruct H3 as [x2'].
    exists (XApp x1 x2'). 
    lets D: EsContext XcApp2; eauto.

   SSCase "x1 steps".
    destruct H2 as [x1'].
    exists (XApp x1' x2).
    lets D: EsContext XcApp1. eauto.
Qed.


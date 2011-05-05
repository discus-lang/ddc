
Require Import EsJudge.
Require Import TyJudge.
Require Import Exp.
Require Import Base.


(* A well typed expression is either a well formed value, 
   or can transition to the next state.
 *)
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
  left. subst. eauto. 

 Case "XApp".
  right. 
  specializes IHTYPE1 Heqtenv.
  specializes IHTYPE2 Heqtenv.
  destruct IHTYPE1.

  SCase "value x1".
   destruct IHTYPE2.
   SSCase "value x2".
    inverts H1. inverts H3.
     inverts H4. false.
     exists (substX 0 x2 x0).
     apply ESLamApp. auto.
   SSCase "x2 steps".
    destruct H2 as [x2'].
    exists (XApp x1 x2'). auto.

   SSCase "x1 steps".
    destruct H1 as [x1'].
    exists (XApp x1' x2). auto.
Qed.



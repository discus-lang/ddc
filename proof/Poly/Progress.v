

Require Import WellFormed.
Require Import EsJudge.
Require Import TyJudge.
Require Import Exp.
Require Import Base.


Theorem Progress
 :  forall x t
 ,  TYPE Empty Empty x t
 -> value x \/ (exists x', STEP x x').
Proof.
 intros.
 induction x.

 Case "XVar".
  inverts H. false.

 Case "XLAM".
  left. apply Value_LAM.
  inverts H.

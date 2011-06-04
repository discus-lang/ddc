

Require Import TyJudge.
Require Import EsJudgeCtx.
Require Import SubstExpExp.

(* Preservation using evaluation judgement with contexts *)
Theorem preservation_ctx
 :  forall x x' t
 ,  TYPE Empty x  t
 -> STEPc x x'
 -> TYPE Empty x' t.
Proof.
 intros. gen t.
 induction H0; intros.

 Case "EsContext".
  destruct H.
  eauto.
  inverts H1. eauto.
  inverts H1. eauto.

 Case "EsLamApp".
  inverts H0.
  inverts H4.
  eapply subst_value_value; eauto.
Qed.
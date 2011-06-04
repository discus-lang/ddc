

Require Import TyJudge.
Require Import EsJudgeCtx.
Require Import SubstExpExp.

(* Preservation using evaluation judgement with contexts *)
Theorem preservation
 :  forall x x' t
 ,  TYPE Empty x  t
 -> STEP  x x'
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


(* When we multi-step evaluate some expression,
   then the result has the same type as the original.
 *)  
Lemma preservation_steps
 :  forall x1 t1 x2
 ,  TYPE Empty x1 t1
 -> STEPS      x1 x2
 -> TYPE Empty x2 t1.
Proof.
 intros. 
 induction H0; eauto.
  eapply preservation; eauto.
Qed.


(* When we multi-step evaluate some expression, 
   then the result has the same type as the original.
   Using the left-linearised form for the evaluation.
 *)
Lemma preservation_stepsl
 :  forall x1 t1 x2
 ,  TYPE Empty x1 t1
 -> STEPSL x1 x2
 -> TYPE Empty x2 t1.
Proof.
 intros. 
 induction H0.
  auto.
  apply IHSTEPSL.
  eapply preservation. 
   eauto. auto.
Qed.

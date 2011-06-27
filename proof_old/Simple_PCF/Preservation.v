

Require Import TyJudge.
Require Import EsJudge.
Require Import SubstExpExp.

(* Preservation using evaluation judgement with contexts *)
Theorem preservation
 :  forall x x' t
 ,  TYPE Empty x  t
 -> STEP  x x'
 -> TYPE Empty x' t.
Proof.
 intros x x' t HT HS. gen t.
 induction HS; intros;
  try (inverts HT; progress eauto).

 Case "EsContext".
  destruct H; eauto
   ; try (inverts HT; eauto).

 Case "EsLamApp".
  inverts HT. inverts H3.
  eapply subst_value_value; eauto.

 Case "EsFix".
  inverts HT.
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

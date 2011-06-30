
Require Import DDC.Language.SimplePCF.Step.
Require Import DDC.Language.SimplePCF.Ty.
Require Import DDC.Language.SimplePCF.SubstExpExp.


(* If a closed, well typed expression takes an evaluation step
   then the result has the same type as before. *)
Theorem preservation
 :  forall x x' t
 ,  TYPE nil x  t
 -> STEP  x x'
 -> TYPE nil x' t.
Proof.
 intros x x' t HT HS. gen t.
 induction HS; intros;
  try (inverts HT; progress eauto).

 Case "EsContext".
  destruct H; eauto
   ; try (inverts HT; eauto).

 Case "EsLamApp".
  inverts HT. inverts H3.
  eapply subst_exp_exp; eauto.

 Case "EsFix".
  inverts HT.
  eapply subst_exp_exp; eauto.
Qed.


(* When we multi-step evaluate some expression,
   then the result has the same type as the before. *)
Lemma preservation_steps
 :  forall x1 t1 x2
 ,  TYPE nil x1 t1
 -> STEPS    x1 x2
 -> TYPE nil x2 t1.
Proof.
 intros. 
 induction H0; eauto.
  eapply preservation; eauto.
Qed.


(* When we multi-step evaluate some expression, 
   then the result has the same type as the original.
   Using the left-linearised form for the evaluation. *)
Lemma preservation_stepsl
 :  forall x1 t1 x2
 ,  TYPE nil x1 t1
 -> STEPSL   x1 x2
 -> TYPE nil x2 t1.
Proof.
 intros. 
 induction H0.
  auto.
  apply IHSTEPSL.
  eapply preservation. 
   eauto. auto.
Qed.

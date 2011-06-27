
Require Import DDC.Language.Simple.Step.
Require Import DDC.Language.Simple.SubstExpExp.
Require Import DDC.Language.Simple.Ty.


(* If a closed, well typed expression takes an evaluation step 
   then the result has the same type as before. *)
Theorem preservation
 :  forall x x' t
 ,  TYPE nil x  t
 -> STEP x x'
 -> TYPE nil x' t.
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
  eapply subst_exp_exp; eauto.
Qed.


(* If a closed, well typed expression takes several evaluation steps
   then the result has the same type as before. *)
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


(* If a closed, well typed expression takes several evaluation steps
   then the result has the same type as before. 
   Usses the left linearised version of steps judement. *)
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



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
 intros x x' t HT HS. gen t.
 induction HS; intros.

 Case "EsContext".
  destruct H; 
   inverts_type; eauto.

 Case "EsLamApp".
  inverts_type.
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
 intros x1 t1 x2 HT HS.
 induction HS; eauto.
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
 intros x1 t1 x2 HT HS.
 induction HS;
  eauto using preservation.
Qed.


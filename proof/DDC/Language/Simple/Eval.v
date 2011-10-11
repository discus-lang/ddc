
Require Export DDC.Language.Simple.SubstExpExp.
Require Import DDC.Language.Simple.Preservation.
Require Import DDC.Language.Simple.Ty.
Require Export DDC.Language.Simple.Step.
Require Export DDC.Language.Simple.Exp.


(********************************************************************)
(** Big Step Evaluation *)
(*  This is also called 'Natural Semantics'.
    It provides a relation between the expression to be reduced 
    and its final value. 
 *)
Inductive EVAL : exp -> exp -> Prop :=
 | EVDone
   :  forall v2
   ,  wnfX  v2
   -> EVAL   v2 v2

 | EVLamApp
   :  forall x1 t11 x12 x2 v2 v3
   ,  EVAL x1 (XLam t11 x12) -> EVAL x2 v2 -> EVAL (substX 0 v2 x12) v3
   -> EVAL (XApp x1 x2) v3.
Hint Constructors EVAL.


(* Invert all hypothesis that are compound eval statements. *)
Ltac inverts_eval :=
 repeat 
  (match goal with 
   | [ H: EVAL (XApp _ _)_ |- _ ] => inverts H
   end).


(* A terminating big-step evaluation always produces a whnf.
   The fact that the evaluation terminated is implied by the fact
   that we have a finite proof of EVAL to pass to this lemma. 
 *)
Lemma eval_produces_whnfX
 :  forall x1 v1
 ,  EVAL   x1 v1
 -> wnfX  v1.
Proof.
 intros. induction H; eauto.
Qed.
Hint Resolve eval_produces_whnfX.


(********************************************************************)
(** * Big to Small steps *)
(* Convert a big-step evaluation into a list of individual
   machine steps.
 *)
Lemma steps_of_eval
 :  forall x1 t1 x2
 ,  TYPE nil x1 t1
 -> EVAL  x1 x2
 -> STEPS x1 x2.
Proof.
 intros x1 t1 v2 HT HE. gen t1.

 (* Induction over the form of (EVAL x1 x2) *)
 induction HE; intros.
 Case "EVDone".
  apply EsNone.

 Case "EVLamApp".
  inverts_type.
  spec IHHE1 H1.
  spec IHHE2 H3.

  lets T1: preservation_steps H1 IHHE1. inverts keep T1.
  lets T2: preservation_steps H3 IHHE2.
  lets T3: subst_exp_exp H0 T2.
  lets E3: IHHE3 T3.

  eapply EsAppend.
    lets D: steps_context XcApp1. eapply D. eauto.
   eapply EsAppend.
    lets D: steps_context XcApp2 IHHE2; eauto.
   eapply EsAppend.
    eapply EsStep.
     eapply EsLamApp.  
      burn.
     eauto.
Qed.


(********************************************************************)
(** * Small to Big steps *)
(** Convert a list of individual machine steps to a big-step
    evaluation. The main part of this is the expansion lemma, which 
    we use to build up the overall big-step evaluation one small-step
    at a time. The other lemmas are used to feed it small-steps.
 *)

(* Given an existing big-step evalution, we can produce a new one
   that does an extra step before returning the original value.
 *)
Lemma eval_expansion
 :  forall te x1 t1 x2 v3
 ,  TYPE te x1 t1
 -> STEP x1 x2 -> EVAL x2 v3 
 -> EVAL x1 v3.
Proof.
 intros te x1 t1 x2 v3 HT HS HE. gen te t1 v3.
 induction HS; intros.

 Case "context".
  destruct H; inverts_type; inverts_eval; eauto; nope.

 Case "application".
  burn.
Qed.


(* Convert a list of small steps to a big-step evaluation. *)
Lemma eval_of_stepsl
 :  forall x1 t1 v2
 ,  TYPE nil x1 t1
 -> STEPSL   x1 v2 -> value v2
 -> EVAL     x1 v2.
Proof.
 intros x1 t1 v2 HT HS Hv.
 induction HS; try burn.

 Case "ESLCons".
  eapply eval_expansion;
   eauto using preservation.
Qed.


(* Convert a multi-step evaluation to a big-step evaluation.
   We use stepsl_of_steps to flatten out the append constructors
   in the multi-step evaluation, leaving a list of individual
   small-steps.
 *)
Lemma eval_of_steps
 :  forall x1 t1 v2
 ,  TYPE nil x1 t1
 -> STEPS x1 v2 -> value v2
 -> EVAL  x1 v2.
Proof.
 intros.
 eapply eval_of_stepsl; eauto.
  apply stepsl_of_steps; auto.
Qed.


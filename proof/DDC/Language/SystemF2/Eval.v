
Require Import DDC.Language.SystemF2.Preservation.
Require Export DDC.Language.SystemF2.Step.
Require Export DDC.Language.SystemF2.SubstExpExp.
Require Export DDC.Language.SystemF2.SubstTypeExp.
Require Import DDC.Language.SystemF2.TyJudge.
Require Export DDC.Language.SystemF2.Exp.


(* Big Step Evaluation.
   This is also called 'Natural Semantics'.
   It provides a relation between the expression to be reduced 
   and its final value. *)
Inductive EVAL : exp -> exp -> Prop :=
 | EVDone
   :  forall v2
   ,  wnfX  v2
   -> EVAL   v2 v2

 | EVLAMAPP
   : forall x1 x12 t2 v3
   ,  EVAL x1 (XLAM x12)     -> EVAL (substTX 0 t2 x12) v3
   -> EVAL (XAPP x1 t2) v3

 | EVLamApp
   :  forall x1 t11 x12 x2 v2 v3
   ,  EVAL x1 (XLam t11 x12) -> EVAL x2 v2 -> EVAL (substXX 0 v2 x12) v3
   -> EVAL (XApp x1 x2) v3.

Hint Constructors EVAL.


(* A terminating big-step evaluation always produces a wnf.
   The fact that the evaluation terminated is implied by the fact
   that we have a finite proof of EVAL to pass to this lemma. *)
Lemma eval_produces_wnfX
 :  forall x1 v1
 ,  EVAL   x1 v1
 -> wnfX  v1.
Proof.
 intros. induction H; eauto.
Qed.
Hint Resolve eval_produces_wnfX.


(* Big to Small steps
   Convert a big-step evaluation into a list of individual
   machine steps. *)
Lemma steps_of_eval
 :  forall x1 t1 x2
 ,  TYPE nil nil x1 t1
 -> EVAL  x1 x2
 -> STEPS x1 x2.
Proof.
 intros x1 t1 v2 HT HE. gen t1.

 (* Induction over the form of (EVAL x1 x2) *)
 induction HE.
 Case "EVDone".
  intros. apply ESNone.

 Case "EVLAMAPP".
  intros. inverts HT.
  lets E1: IHHE1 H3. clear IHHE1.
  lets T1: preservation_steps H3 E1. inverts keep T1.
  lets T2: subst_type_exp H4 H5.
   simpl in T2.
  lets E2: IHHE2 T2.
  eapply ESAppend.
   apply steps_APP1. eauto.
  eapply ESAppend.
   eapply ESStep.
    eapply ESLAMAPP. auto.

 Case "EVLamApp".
  intros. inverts HT.
  lets E1: IHHE1 H3. 
  lets E2: IHHE2 H5.
  lets T1: preservation_steps H3 E1. inverts keep T1.
  lets T2: preservation_steps H5 E2.
  lets T3: subst_exp_exp H8 T2.
  lets E3: IHHE3 T3.
  eapply ESAppend.
    eapply steps_app1. eauto.
   eapply ESAppend.
    eapply steps_app2. eauto. eauto.
   eapply ESAppend.
    eapply ESStep.
     eapply ESLamApp. eauto.
   eauto.      
Qed.


(* Small to Big steps
   Convert a list of individual machine steps to a big-step
   evaluation. The main part of this is the expansion lemma, which 
   we use to build up the overall big-step evaluation one small-step
   at a time. The other lemmas are used to feed it small-steps.
 *)

(* Given an existing big-step evalution, we can produce a new one
   that does an extra step before returning the original value.
 *)
Lemma eval_expansion
 :  forall ke te x1 t1 x2 v3
 ,  TYPE ke te x1 t1
 -> STEP x1 x2 -> EVAL x2 v3 
 -> EVAL x1 v3.
Proof.
 intros. gen ke te t1 v3.

 (* Induction over the form of (STEP x1 x2) *)
 induction H0; intros.

 Case "XApp".
  SCase "value app".
   eapply EVLamApp.
   eauto.
   apply EVDone. auto. auto.
 
  SCase "x1 steps".
   inverts H. inverts H1.
    inverts H.
    eapply EVLamApp; eauto.

  SCase "x2 steps".
   inverts H1. inverts H2.
   inverts H1.
   eapply EVLamApp; eauto.

 Case "XAPP".
  SCase "type app".
   eapply EVLAMAPP.
   eauto.
   inverts H.
   auto.

  SCase "x1 steps".
   inverts H. inverts H1.
    inverts H.
    eapply EVLAMAPP; eauto.
Qed.


(* Convert a list of small steps to a big-step evaluation. *)
Lemma eval_of_stepsl
 :  forall x1 t1 v2
 ,  TYPE nil nil x1 t1
 -> STEPSL x1 v2 -> value v2
 -> EVAL   x1 v2.
Proof.
 intros.
 induction H0.
 
 Case "ESLNone".
   apply EVDone. inverts H1. auto.

 Case "ESLCons".
  eapply eval_expansion. 
   eauto. eauto. 
   apply IHSTEPSL.
   eapply preservation. eauto. auto. auto.
Qed.


(* Convert a multi-step evaluation to a big-step evaluation.
   We use stepsl_of_steps to flatten out the append constructors
   in the multi-step evaluation, leaving a list of individual
   small-steps.
 *)
Lemma eval_of_steps
 :  forall x1 t1 v2
 ,  TYPE nil nil x1 t1
 -> STEPS x1 v2 -> value v2
 -> EVAL  x1 v2.
Proof.
 intros.
 eapply eval_of_stepsl; eauto.
 apply  stepsl_of_steps; auto.
Qed.


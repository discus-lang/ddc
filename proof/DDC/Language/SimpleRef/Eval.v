
Require Export DDC.Language.SimpleRef.SubstExpExp.
Require Import DDC.Language.SimpleRef.Preservation.
Require Import DDC.Language.SimpleRef.Ty.
Require Export DDC.Language.SimpleRef.Step.
Require Export DDC.Language.SimpleRef.Exp.


(********************************************************************)
(** Big Step Evaluation *)
(*  This is also called 'Natural Semantics'.
    It provides a relation between the expression to be reduced 
    and its final value. 
 *)
Inductive EVAL : heap -> exp -> heap -> exp -> Prop :=
 | EVDone
   :  forall h v2
   ,  wnfX  v2
   -> EVAL  h v2 h v2

 | EVLamApp
   :  forall h0 h1 h2 h3 x1 t11 x12 x2 v2 v3
   ,  EVAL h0 x1                h1 (XLam t11 x12)
   -> EVAL h1 x2                h2 v2
   -> EVAL h2 (substX 0 v2 x12) h3 v3
   -> EVAL h0 (XApp x1 x2)      h3 v3.

Hint Constructors EVAL.


(* Invert all hypothesis that are compound eval statements *)
Ltac inverts_eval := 
 repeat 
  (match goal with 
    | [ H: EVAL _ (XApp _ _) _ _ |- _ ] => inverts H
   end).


(* A terminating big-step evaluation always produces a wnf.
   The fact that the evaluation terminated is implied by the fact
   that we have a finite proof of EVAL to pass to this lemma. 
 *)
Lemma eval_produces_wnfX
 :  forall h0 x1 h1 v1
 ,  EVAL   h0 x1 h1 v1
 -> wnfX   v1.
Proof.
 intros. induction H; eauto.
Qed.
Hint Resolve eval_produces_wnfX.


(********************************************************************)
(** * Big to Small steps *)
(* Convert a big-step evaluation into a list of individual
   machine steps.
 *)
Lemma steps_of_eval
 :  forall se h0 h1 x1 t1 x2
 ,  TYPEH se  h0
 -> TYPE  nil se x1 t1
 -> EVAL  h0  x1 h1 x2
 -> STEPS h0  x1 h1 x2.
Proof.
 intros se h0 h1 x1 t1 v2 HTH HT HE. gen se t1.

 (* Induction over the form of (EVAL x1 x2) *)
 induction HE; intros.
 Case "EVDone".
  apply EsNone.

 Case "EVLamApp".
  inverts_type. 
   rename H3 into Tx1.
   rename H5 into Tx2.

  (* evaluate function *)
  assert (STEPS h0 x1 h1 (XLam t11 x12)) as Sx1.
   eauto. clear IHHE1.
  lets Rx1: preservation_steps HTH Tx1 Sx1.
   destruct Rx1 as [se1]. int.
   inverts_type. 

  (* evaluate arg *)
  assert (TYPE  nil se1 x2 t0). iauto.
  assert (STEPS h1  x2  h2 v2) as Sx2.
   iauto. clear IHHE2.
  lets Rx1: preservation_steps se1 h1 x2 t0 h2.
  lets Rx1': Rx1 v2. clear Rx1. int.
  destruct H2 as [se2]. int.

  (* perform substitution *)
  assert (TYPE  nil se2 (substX 0 v2 x12) t1).
   eapply subst_exp_exp; eauto.
  assert (STEPS h2 (substX 0 v2 x12) h3 v3). eauto.
  lets Rx2: preservation_steps se2 h2 (substX 0 v2 x12) t1 h3.
  lets Rx2': Rx2 v3. clear Rx2. int.
  destruct H8 as [se3]. int.

  eapply EsAppend.
    lets D: steps_context XcApp1.
     eapply D. eauto.
  eapply EsAppend.
   lets D: steps_context XcApp2.
    assert (wnfX (XLam t0 x12)) as WL. auto. eapply WL.
    eapply D. eauto.
  eapply EsAppend.
   eapply EsStep.
    eapply EsLamApp. eauto.
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
(*
Lemma eval_expansion
 :  forall te x1 t1 x2 v3
 ,  TYPE te x1 t1
 -> STEP x1 x2 -> EVAL x2 v3 
 -> EVAL x1 v3.
Proof.
 intros. gen te t1 v3.

 (* Induction over the form of (STEP x1 x2) *)
 induction H0; intros.

 Case "eval in context".
  destruct H. 

  SCase "top level".
   eauto.
 
  SCase "x1 steps".
   inverts H1. inverts H2.
    inverts H.
    eapply EVLamApp; eauto.

  SCase "x2 steps".
   inverts H1. inverts H2.
   inverts H1.
   eapply EVLamApp; eauto.

  SCase "application".
   eapply EVLamApp.
    eauto.
    inverts H. 
    apply EVDone. auto. auto.
Qed.


(* Convert a list of small steps to a big-step evaluation. *)
Lemma eval_of_stepsl
 :  forall x1 t1 v2
 ,  TYPE nil x1 t1
 -> STEPSL   x1 v2 -> value v2
 -> EVAL     x1 v2.
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
 ,  TYPE nil x1 t1
 -> STEPS x1 v2 -> value v2
 -> EVAL  x1 v2.
Proof.
 intros.
 eapply eval_of_stepsl; eauto.
 apply  stepsl_of_steps; auto.
Qed.
*)


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
 | EvDone
   :  forall h v2
   ,  value  v2
   -> wfH    h
   -> EVAL   h v2 h v2

 | EvLamApp
   :  forall h0 h1 h2 h3 x1 t11 x12 x2 v2 v3
   ,  EVAL h0 x1                h1 (XLam t11 x12)
   -> EVAL h1 x2                h2 v2
   -> EVAL h2 (substX 0 v2 x12) h3 v3
   -> EVAL h0 (XApp x1 x2)      h3 v3

 (* Heap operations *)
 | EvNewRef
   :  forall h0 x1 h1 v1
   ,  EVAL h0 x1      h1 v1         (* evaluate new heap value *)
   -> EVAL h0        (XNewRef x1)   (* push that value onto the heap *)
          (v1 <: h1) (XLoc (length h1))

 | EvReadRef
   :  forall h0 x1 h1 v2 l
   ,  EVAL h0 x1 h1 (XLoc l)        (* evaluate heap location *)
   -> get l h1 = Some v2            (* lookup up that location from the heap *)
   -> EVAL h0 (XReadRef x1) 
           h1 v2

 | EvWriteRef
   :  forall h0 x1 h1 x2 v2 h2 l
   ,  EVAL h0 x1 h1 (XLoc l)        (* evaluate heap location *)
   -> EVAL h1 x2 h2 v2              (* evaluate argument to a value *)
   -> EVAL h0    (XWriteRef x1 x2)  
           (update l v2 h2) xUnit.  (* update heap with that value*)

Hint Constructors EVAL.


(* Invert all hypothesis that are compound eval statements *)
Ltac inverts_eval := 
 repeat 
  (match goal with 
    | [ H: EVAL _ (XLoc _)        _ _ |- _ ] => inverts H
    | [ H: EVAL _ (XCon _)        _ _ |- _ ] => inverts H
    | [ H: EVAL _ (XLam _ _)      _ _ |- _ ] => inverts H
    | [ H: EVAL _ (XApp _ _)      _ _ |- _ ] => inverts H
    | [ H: EVAL _ (XNewRef _)     _ _ |- _ ] => inverts H
    | [ H: EVAL _ (XReadRef _)    _ _ |- _ ] => inverts H
    | [ H: EVAL _ (XWriteRef _ _) _ _ |- _ ] => inverts H
   end).


Lemma eval_value_eq
 :  forall h0 v0 h1 v1
 ,  value v0
 -> EVAL h0 v0 h1 v1
 -> h1 = h0 /\ v1 = v0.
Proof.
 intros. 
 destruct v0; inverts_eval; nope; eauto.
Qed.


(* A terminating big-step evaluation always produces a wnf, 
   and preserves the wellformedness of the heap. 
   The fact that the evaluation terminated is implied by the fact
   that we have a finite proof of EVAL to pass to this lemma. *)
Lemma eval_produces_wfH_value
 :  forall h0 x1 h1 v1
 ,  EVAL   h0 x1 h1 v1
 -> wfH h1 /\ value v1.
Proof.
 intros. induction H; rip; try burn.
 apply Forall_snoc; eauto.
 eapply Value; burn.
 eapply Forall_get; eauto.
 eapply Forall_update; auto.
 unfold xUnit.
 eapply Value; burn.
Qed.


(* A terminating big-step evaluation produces a well formed heap. *)
Lemma eval_produces_wfH
 :  forall h0 x1 h1 v1
 ,  EVAL h0 x1 h1 v1
 -> wfH h1.
Proof.
 intros.
 lets D: eval_produces_wfH_value H. tauto.
Qed.
Hint Resolve eval_produces_wfH.


(* A terminating big-step evaluation produces a value. *)
Lemma eval_produces_value
 :  forall h0 x1 h1 v1
 ,  EVAL h0 x1 h1 v1
 -> value v1.
Proof.
 intros.
 lets D: eval_produces_wfH_value H. tauto.
Qed.
Hint Resolve eval_produces_value.



(********************************************************************)
(** * Big to Small steps *)
(* Flatten out a big-step evaluation into a list of individual
   machine steps.

   Proof: 
    This is tedious but straightforward. We assert each of the 
    intermediate STEPS to get the intermediate values, then use
    preservation to show the results have the same type as before.
    Once we've got a STEPS for each of the hyps of the EVAL we're
    we're flattening, join them all together with an EsAppend,
    stating the contexts we're performing the reductions in.
 *)
Lemma steps_of_eval
 :  forall se h0 h1 x1 t1 x2
 ,  wfH h0 -> TYPEH se h0
 -> TYPE  nil se x1 t1
 -> EVAL  h0  x1 h1 x2
 -> STEPS h0  x1 h1 x2.
Proof.
 intros se h0 h1 x1 t1 v2 HTW HTH HT HE. gen se t1.

 (* Induction over the form of (EVAL x1 x2) *)
 induction HE; intros.
 Case "EvDone".
  apply EsNone.

 (* Function Application **********)
 Case "EvLamApp".
  inverts_type. 
   rename H3 into Tx1.
   rename H5 into Tx2.

  (* evaluate function *)
  assert (STEPS h0 x1 h1 (XLam t11 x12)) as Sx1.
   eauto. clear IHHE1.
  lets Rx1: preservation_steps HTH Tx1 Sx1.
   destruct Rx1 as [se1]. rip.
   inverts keep H2.

  (* evaluate arg *)
  assert (TYPE  nil se1 x2 t0). burn.
  assert (STEPS h1  x2  h2 v2) as Sx2.
   burn. clear IHHE2.
  lets Rx1: preservation_steps se1 h1 x2 t0 h2.
  lets Rx1': Rx1 v2. clear Rx1. rip.
  destruct Rx1' as [se2]. rip.

  (* perform substitution *)
  assert (TYPE  nil se2 (substX 0 v2 x12) t1).
   eapply subst_exp_exp; eauto.
  assert (STEPS h2 (substX 0 v2 x12) h3 v3). eauto.
  lets Rx2: preservation_steps se2 h2 (substX 0 v2 x12) t1 h3.
  lets Rx2': Rx2 v3. clear Rx2. rip.
  destruct Rx2' as [se3]. rip.

  eapply EsAppend.
    lets D: steps_context XcApp1.
     eapply D. eauto.
  eapply EsAppend.
   lets D: steps_context XcApp2.
    assert (wnfX (XLam t0 x12)) as WL. auto.
     assert (closedX (XLam t0 x12)). 
     eauto. eauto. eapply D. eauto.

  eapply EsAppend.
   eapply EsStep.
    eapply EsLamApp. eauto.
    eauto.


 (* Create a new Reference *******)
 Case "EvNewRef".
  inverts_type.
   rename H2 into Tx1.
  
  (* evaluate argument *)
  assert (STEPS h0 x1 h1 v1) as Sx1.
   eauto. clear IHHE.
  lets Rx1: preservation_steps HTH Tx1 Sx1.
   destruct Rx1 as [se1]. rip.
  
  eapply EsAppend.
   lets D: steps_context XcNewRef.
    eapply D. eauto.
  eapply EsStep. eauto.


 (* Read a reference ************)
 Case "EvReadRef".
  inverts_type.
   rename H3 into Tx1.
  
  (* evaluate argument to a location *)  
  assert (STEPS h0 x1 h1 (XLoc l)) as Sx1.
   eauto. clear IHHE.
  lets Rx1: preservation_steps HTH Tx1 Sx1.
   destruct Rx1 as [se1]. rip.
 
  eapply EsAppend.
   lets D: steps_context XcReadRef.
    eapply D. eauto.
  eapply EsStep. eauto.


 (* Write a reference **********)
 Case "EvWriteRef".
  inverts_type.
   rename H3 into Tx1.
   rename H5 into Tx2.

  (* evaluate first argument to a location *)
  assert (STEPS h0 x1 h1 (XLoc l)) as Sx1.
   eauto. clear IHHE1.
  lets Rx1: preservation_steps HTH Tx1 Sx1.
   destruct Rx1 as [se1]. rip.

  (* evaluate second argument to a value *)
  assert (STEPS h1 x2 h2 v2) as Sx2.
   eauto. clear IHHE2.
  assert (TYPE nil se1 x2 tData) as Tx2'. 
   eauto.
  lets Rx2: preservation_steps H Tx2' Sx2.
   destruct Rx2 as [se2]. rip.

  eapply EsAppend.
   lets D: steps_context XcWriteRef1. 
    eapply D. eapply Sx1.
  eapply EsAppend.
   lets D: steps_context XcWriteRef2.
    assert (value (XLoc l)). eauto. eauto.
    eapply D. eapply Sx2.
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
 :  forall se h1 x1 t1 h2 x2 h3 v3
 ,  wfH h1 -> TYPEH se h1
 -> TYPE  nil se x1 t1
 -> STEP  h1 x1  h2 x2 -> EVAL h2 x2  h3 v3 
 -> EVAL  h1 x1  h3 v3.
Proof.
 intros se h1 x1 t1 h2 x2 h3 v3 HW HTH HT HS HE.
 gen se t1 h3 v3.

 (* Induction over the form of (STEP x1 x2) *)
 induction HS; intros.

 Case "context".
  destruct H; eauto; inverts_type.
  SCase "XcApp".
   inverts_eval. nope. eauto.
   inverts_eval. nope.
    assert (h1 = h' /\ XLam t11 x12 = v1).
    eapply eval_value_eq; eauto. rip.
    eapply EvLamApp. eauto. eauto.
    eauto.
 
  SCase "XcNewRef".
   inverts_eval. simpl. nope. eauto.

  SCase "XcReadRef".
   inverts_eval. nope. eauto.

  SCase "XcWriteRef".
   inverts_eval. nope. eauto. eauto.
   inverts_eval. nope. eauto.
    assert (h1 = h' /\ XLoc l0 = v1).
    eapply eval_value_eq; eauto. rip.
    eauto.

 Case "XApp".
  inverts_type.
  eapply EvLamApp; eauto.
  assert (value (XLam t0 x12)). eauto.
  eauto.

 Case "XNewRef".
  assert (h3 = v1 <: h /\ v3 = XLoc (length h)).
   eapply eval_value_eq; eauto. eapply Value; burn. 
   rip.

 Case "XReadRef".
  assert (value v).
   eapply Forall_get.
    eapply HW. eapply H.
  assert (h3 = h /\ v3 = v).
   eapply eval_value_eq; eauto. rip.
  eapply EvReadRef; eauto.
  eapply EvDone. eapply Value; burn.
   auto.

 Case "XWriteRef".
  assert (h3 = update l v2 h /\ v3 = xUnit).
   eapply eval_value_eq; eauto. unfold xUnit. 
   eapply Value; burn. rip.
  eauto.
  eapply EvWriteRef.
  eapply EvDone. eapply Value; burn.
  auto.
  inverts HE.
   inverts_type.
   assert (value v2). 
    assert (exists xData, get l h = Some xData).
    eapply Forall2_get_get_right.
     eapply HTH. eapply H7. destruct H2.
    eapply Forall_update_result.
     eapply H2. auto.
   eauto.
Qed.


(* Convert a list of small steps to a big-step evaluation. *)
Lemma eval_of_stepsl
 :  forall se h1 x1 t1 h2 v2
 ,  wfH h1 -> TYPEH  se h1
 -> TYPE   nil se x1 t1
 -> STEPSL h1 x1 h2 v2 -> value v2
 -> EVAL   h1 x1 h2 v2.
Proof.
 intros. gen se.
 induction H2; intros.
 
 Case "EslNone".
   apply EvDone; auto.

 Case "EslCons".
  eapply eval_expansion; eauto.
  lets D: preservation H1 H4 H0.
  dest D. rip.
  assert (wfH h2). 
   eapply step_preserves_wfH; eauto.
  rip.
  eauto.
Qed.


(* Convert a multi-step evaluation to a big-step evaluation.
   We use stepsl_of_steps to flatten out the append constructors
   in the multi-step evaluation, leaving a list of individual
   small-steps.
 *)
Lemma eval_of_steps
 :  forall se h1 x1 t1 h2 v2
 ,  wfH h1 -> TYPEH se h1
 -> TYPE nil se x1 t1
 -> STEPS h1 x1 h2 v2 -> value v2
 -> EVAL  h1 x1 h2 v2.
Proof.
 intros.
 eapply eval_of_stepsl; eauto.
 apply  stepsl_of_steps; auto.
Qed.


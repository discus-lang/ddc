
Require Export SubstExpExp.
Require Export SubstTypeExp.
Require Import Preservation.
Require Import TyJudge.
Require Export EsJudge.
Require Export Exp.


(* Big Step Evaluation **********************************************
   This is also called 'Natural Semantics'.
   It provides a relation between the expression to be reduced 
   and its final value. 
 *)
Inductive EVAL : exp -> exp -> Prop :=
 | EVDone
   :  forall v2
   ,  whnfX  v2
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


(* A terminating big-step evaluation always produces a whnf.
   The fact that the evaluation terminated is implied by the fact
   that we have a finite proof of EVAL to pass to this lemma. 
 *)
Lemma eval_produces_whnfX
 :  forall x1 v1
 ,  EVAL   x1 v1
 -> whnfX  v1.
Proof.
 intros. induction H; eauto.
Qed.
Hint Resolve eval_produces_whnfX.


(* Big to Small steps ***********************************************
   Convert a big-step evaluation into a list of individual
   machine steps.
 *)
Lemma steps_of_eval
 :  forall x1 t1 x2
 ,  TYPE Empty Empty x1 t1
 -> EVAL x1 x2
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
  lets T2: subst_type_value H4 H5.
   simpl in T2.
    assert (substTE 0 t2 (liftTE 0 Empty) = Empty). auto.
    rewrite H in T2. clear H.
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
  lets T3: subst_value_value H8 T2.
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

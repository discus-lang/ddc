
Require Import SubstTypeExp.
Require Import SubstExpExp.
Require Export Exp.


(* Single Step Evaluation *******************************************)
Inductive STEP : exp -> exp -> Prop := 
  (* value applications *)
  | ESLamApp
    :  forall t11 x12 v2
    ,  value v2
    -> STEP (XApp (XLam t11 x12) v2) (substXX 0 v2 x12)

  | ESApp1
    :  forall x1 x1' x2
    ,  STEP x1 x1'
    -> STEP (XApp x1 x2) (XApp x1' x2)
 
  | ESApp2
    :  forall v1 x2 x2'
    ,  value v1
    -> STEP x2 x2'
    -> STEP (XApp v1 x2) (XApp v1 x2')

  (* type applications *)
  | ESLAMAPP
    :  forall x12 t2      
    ,  STEP (XAPP (XLAM x12) t2) (substTX 0 t2 x12)

  | ESAPP1
    :  forall x1 x1' t2
    ,  STEP x1 x1'
    -> STEP (XAPP x1 t2) (XAPP x1' t2).

Hint Constructors STEP.


(* Multi-step evaluation *******************************************
   A sequence of small step transitions.
   As opposed to STEPSL, this version has an append constructor
   ESAppend that makes it easy to join two evaluations together.
   We use this when converting big-step evaluations to small-step.
 *)
Inductive STEPS : exp -> exp -> Prop :=

 (* After no steps, we get the same exp.
    We need this constructor to match the EVDone constructor
    in the big-step evaluation, so we can convert between big-step
    and multi-step evaluations. *)
 | ESNone
   :  forall x1
   ,  STEPS x1 x1

 (* Take a single step. *)
 | ESStep
   :  forall x1 x2
   ,  STEP  x1 x2
   -> STEPS x1 x2

 (* Combine two evaluations into a third. *)
 | ESAppend
   :  forall x1 x2 x3
   ,  STEPS x1 x2 -> STEPS x2 x3
   -> STEPS x1 x3.

Hint Constructors STEPS.


(* Context lemmas ***************************************************
   These help when proving something about a reduction in a given
   context. They're all trivial.
 *)
Lemma steps_app1
 :  forall x1 x1' x2
 ,  STEPS x1 x1'
 -> STEPS (XApp x1 x2) (XApp x1' x2).
Proof.
 intros. induction H; eauto.
Qed.
Hint Resolve steps_app1.


Lemma steps_app2
 :  forall v1 x2 x2'
 ,  value v1 
 -> STEPS x2 x2'
 -> STEPS (XApp v1 x2) (XApp v1 x2').
Proof.
 intros. induction H0; eauto.
Qed.
Hint Resolve steps_app2.


Lemma steps_APP1
 :  forall x1 x1' t2
 ,  STEPS x1 x1'
 -> STEPS (XAPP x1 t2) (XAPP x1' t2).
Proof.
 intros. induction H; eauto.
Qed.


(* Left linearised multi-step evaluation ****************************
   As opposed to STEPS, this version provides a single step at a time
   and does not have an append constructor. This is convenient
   when converting a small-step evaluations to big-step, via the
   eval_expansion lemma.
 *)
Inductive STEPSL : exp -> exp -> Prop :=

 | ESLNone 
   : forall x1
   , STEPSL x1 x1

 | ESLCons
   :  forall x1 x2 x3
   ,  STEP   x1 x2 -> STEPSL x2 x3 
   -> STEPSL x1 x3.

Hint Constructors STEPSL.


(* Transitivity of left linearised multi-step evaluation.
   We use this when "flattening" a big step evaluation to the
   small step one.
 *)
Lemma stepsl_trans
 :  forall x1 x2 x3
 ,  STEPSL x1 x2 -> STEPSL x2 x3
 -> STEPSL x1 x3.
Proof.
 intros.
 induction H.
  eauto.
  eapply ESLCons. eauto. eauto.
Qed.


(* Linearise a regular multi-step evaluation.
   This flattens out all the append constructors, leaving us with
   a list of individual transitions. 
 *)
Lemma stepsl_of_steps
 :  forall x1 x2
 ,  STEPS  x1 x2
 -> STEPSL x1 x2.
Proof. 
 intros.
 induction H.
  auto.
  eauto.
  eapply stepsl_trans; eauto.
Qed.



Require Export Exp.
Require Export SubstValueValue.


(* Single Small Step Evaluation *************************************
   The single step rules model the individual transitions that the 
   machine can make at runtime.
 *)
Inductive STEP : exp -> exp -> Prop :=
 | ESLamApp
   : forall t11 x12 v2
   ,  value v2
   -> STEP (XApp   (XLam t11 x12) v2)
           (subst v2 x12)

 | ESApp1 
   :  forall x1 x1' x2
   ,  STEP x1 x1'
   -> STEP (XApp x1 x2) (XApp x1' x2)

 | ESApp2 
   :  forall v1 x2 x2'
   ,  value v1
   -> STEP x2 x2'
   -> STEP (XApp v1 x2) (XApp v1 x2').

Hint Constructors STEP.


(* Multi-step evaluation *******************************************
   A sequence of small step transitions.
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


(* Left linearised multi-step evaluation ****************************)
Inductive STEPSL : exp -> exp -> Prop :=

 | ESLNone 
   : forall x1
   , STEPSL x1 x1

 | ESLSnoc 
   :  forall x1 x2 x3
   ,  STEP   x1 x2 -> STEPSL x2 x3 
   -> STEPSL x1 x3.

Hint Constructors STEPSL.


(* Transitivity of left linearised multi-step evaluation. *)
Lemma stepsl_trans
 :  forall x1 x2 x3
 ,  STEPSL x1 x2 -> STEPSL x2 x3
 -> STEPSL x1 x3.
Proof.
 intros.
 induction H.
  eauto.
  eapply ESLSnoc. eauto. eauto.
Qed.


(* Linearise a regular multi-step evaluation. *)
Lemma steps_to_stepsl
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
  



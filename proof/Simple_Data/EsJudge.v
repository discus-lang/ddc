
Require Export EsAlt.
Require Export EsContext.
Require Export TyJudge.
Require Export Exp.
Require Export SubstExpExp.



(********************************************************************)
(** * Single Small Step Evaluation *)
(** The single step rules model the individual transitions that the 
     machine can make at runtime. *)

Inductive STEP : exp -> exp -> Prop :=

 (* Step some sub-expression in an evaluation context *)
 | EsContext 
   :  forall C x x'
   ,  exp_ctx C
   -> STEP x x'
   -> STEP (C x) (C x')

 | EsLamApp
   : forall t11 x12 v2
   ,  whnfX v2
   -> STEP (XApp   (XLam t11 x12) v2)
           (substX 0 v2 x12)

 | EsCaseAlt
   :  forall dc vs tsArgs alts x
   ,  Forall whnfX vs
   -> getAlt dc alts = Some (AAlt dc tsArgs x)
   -> STEP (XCase (XCon dc vs) alts)
           (substXs 0 vs x).

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
 | EsNone
   :  forall x1
   ,  STEPS x1 x1

 (* Take a single step. *)
 | EsStep
   :  forall x1 x2
   ,  STEP  x1 x2
   -> STEPS x1 x2

 (* Combine two evaluations into a third. *)
 | EsAppend
   :  forall x1 x2 x3
   ,  STEPS x1 x2 -> STEPS x2 x3
   -> STEPS x1 x3.

Hint Constructors STEPS.


Lemma steps_context
 :  forall C x1 x1'
 ,  exp_ctx C
 -> STEPS x1 x1'
 -> STEPS (C x1) (C x1').
Proof.
 intros.
 induction H0.
  auto.
  auto.
  eapply EsAppend; eauto.
Qed.


Lemma steps_context_XCon
 :  forall C x v dc
 ,  exps_ctx C
 -> STEPS x v
 -> STEPS (XCon dc (C x)) (XCon dc (C v)).
Proof.
 intros.
 induction H0.
  auto.
  lets D: EsContext XcCon. eauto. eauto.
  eapply EsAppend.
   eapply IHSTEPS1.
   eauto.
Qed.


(* Left linearised multi-step evaluation ****************************
   As opposed to STEPS, this version provides a single step at a time
   and does not have an append constructor. This is convenient
   when converting a small-step evaluations to big-step, via the
   eval_expansion lemma.
 *)
Inductive STEPSL : exp -> exp -> Prop :=
 | EslNone 
   : forall x1
   , STEPSL x1 x1

 | EslCons
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
  eapply EslCons; eauto.
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


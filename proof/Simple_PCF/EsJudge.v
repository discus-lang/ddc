
Require Export Exp.
Require Export SubstExpExp.


(* Single Small Step Evaluation *************************************
   The single step rules model the individual transitions that the 
   machine can make at runtime.
 *)
Inductive STEP : exp -> exp -> Prop :=
 (* Applications **********************)
 (* Evaluate the left of the application to get the abstraction. *)
 | ESApp1 
   :  forall x1 x1' x2
   ,  STEP x1 x1'
   -> STEP (XApp x1 x2) (XApp x1' x2)

 (* Evaluate the right of the application to get a value. *)
 | ESApp2 
   :  forall v1 x2 x2'
   ,  value v1
   -> STEP x2 x2'
   -> STEP (XApp v1 x2) (XApp v1 x2')

 (* Substitute value into the abstraction. *) 
 | ESLamApp
   : forall t11 x12 v2
   ,  value v2
   -> STEP (XApp   (XLam t11 x12) v2)
           (substX 0 v2 x12)

 (* Fixpoints *************************)
 (* Substitute the abstraction into itself. *)
 | ESFix
   :  forall t11 x12
   ,  STEP (XFix t11 x12)
           (substX 0 (XFix t11 x12) x12)

 (* Naturals **************************)
 (* Reduce the argument of a Succ to a value, so that the Succ
    constructor is strict. Alternatively we could just leave it. *)
 | ESSuccCtx
   :  forall x1 x1'
   ,  STEP x1 x1'
   -> STEP (XSucc x1) (XSucc x1')
 
 (* Increment the primitive value *)
 | ESSucc
   :  forall n
   ,  STEP (XSucc (XNat n)) (XNat (S n))

 (* Reduce the argument of a Pred to a value. *)
 | ESPredCtx
   :  forall x1 x1'
   ,  STEP x1 x1'
   -> STEP (XPred x1) (XPred x1')

 (* If we've got a Zero then just return Zero, 
    this way we don't need to worry about negative naturals. *)
 | ESPredZero 
   :  STEP (XPred (XNat O)) (XNat O)

 (* If we've got a Succ then return the inner expression. *)
 | ESPredSucc 
   :  forall n
   ,  STEP (XPred (XNat (S n))) (XNat n)

 (* Booleans **************************)
 (* Reduce the argument of IsZero to a value. *)
 | ESIsZeroCtx
   :  forall x1 x1'
   ,  STEP x1 x1'
   -> STEP (XIsZero x1) (XIsZero x1')
 
 | ESIsZeroTrue
   :  STEP (XIsZero (XNat O)) XTrue

 | ESIsZeroFalse
   :  forall n
   ,  STEP (XIsZero (XNat (S n))) XFalse

 (* Branching *************************)
 (* Reduce the discriminant to a value,
    we'll get a True or a False *)
 | ESIfCtx
   :  forall x1 x1' x2 x3 
   ,  STEP x1 x1'
   -> STEP (XIf x1 x2 x3) (XIf x1' x2 x3)

 (* Take the 'then' branch. *)
 | ESIfThen
   :  forall x2 x3
   ,  STEP (XIf XTrue x2 x3) x2

 (* Take the 'else' branch. *)
 | ESIfElse
   :  forall x2 x3
   ,  STEP (XIf XFalse x2 x3) x3.

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


Lemma steps_succ
 :  forall x1 x1'
 ,  STEPS x1 x1'
 -> STEPS (XSucc x1) (XSucc x1').
Proof.
 intros. induction H; eauto.
Qed.
Hint Resolve steps_succ.


Lemma steps_pred
 :  forall x1 x1'
 ,  STEPS x1 x1'
 -> STEPS (XPred x1) (XPred x1').
Proof.
 intros. induction H; eauto.
Qed.
Hint Resolve steps_pred.


Lemma steps_isZero
 :  forall x1 x1'
 ,  STEPS x1 x1'
 -> STEPS (XIsZero x1) (XIsZero x1').
Proof.
 intros. induction H; eauto.
Qed.
Hint Resolve steps_isZero.


Lemma steps_if
 :  forall x1 x1' x2 x3
 ,  STEPS x1 x1'
 -> STEPS (XIf x1 x2 x3) (XIf x1' x2 x3).
Proof.
 intros. induction H; eauto.
Qed.
Hint Resolve steps_if.


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


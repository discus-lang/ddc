
Require Export Exp.
Require Export SubstExpExp.


(********************************************************************)
(** * Evaluation Contexts *)
(** An evaluation context defines a function that allows us to
    update a node in the AST for an expression. We use this to
    update nodes during single step evaluation. 

    Another way of viewing it is that the exp_ctx data type provides
    a mechanism to describe a particular point in the AST. Using the 
    exp_constructors, we can name any sub-expression that is ready
    to take a step. *)
Inductive exp_ctx : (exp -> exp) -> Prop :=

 (* The top level context names the entire expression *)
 | XcTop 
   : exp_ctx  (fun x => x)

 (* Left of an application *)
 | XcApp1
   :  forall x2
   ,  exp_ctx  (fun xx => XApp xx x2)

 (* The right of an application can step only when the left is
    already a value. *)
 | XcApp2 
   :  forall v1
   ,  value v1
   -> exp_ctx  (fun xx => XApp v1 xx)

 (* As the XCon constructor contains a list of sub-expressions, 
    we need an additional exps_ctx context to indicate which one 
    we're talking about. *)
 | XcCon 
   :  forall dc C
   ,  exps_ctx C
   -> exp_ctx  (fun xx => XCon dc (C xx))
 
 (* Some sub-expression in a list can only take a step when all 
    the previous expressions are already values. *)
 with exps_ctx : (exp -> list exp) -> Prop :=
  | XscHead
    : forall xs
    , exps_ctx (fun xx => xx :: xs)

  | XscTail 
    :  forall v1 C
    ,  value v1
    -> exps_ctx C
    -> exps_ctx (fun xx => v1 :: C xx).


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
   ,  value v2
   -> STEP (XApp   (XLam t11 x12) v2)
           (substX 0 v2 x12).


(* TODO: Add rule for case expressions, 
         need to choose the correct alternative, then do the
         substitutions *)

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



Require Export Exp.
Require Export SubstValueValue.


(* Single Small Step Evaluation *************************************
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
   This is a sequence of small step evaluations.
 *)
Inductive STEPS : nat -> exp -> exp -> Prop :=

 (* After no steps, we get the same exp.
    We need this constructor to match the EVValue constructor
    in the big-step evaluation, so we can convert between big-step
    and mult-step evaluations. *)
 | ESDone
   :  forall x1
   ,  STEPS 0 x1 x1

 (* Take a single step. *)
 | ESStep
   :  forall x1 x2
   ,  STEP  x1 x2
   -> STEPS 1 x1 x2

 (* Combine two reductions into a third. *)
 | ESLink
   :  forall n1 n2 x1 x2 x3
   ,  STEPS n1 x1 x2 -> STEPS n2 x2 x3
   -> STEPS (n1 + n2) x1 x3.

Hint Constructors STEPS.


(* Context lemmas ***************************************************
   These help when proving something about a reduction in a given
   context. They're all trivial.
 *)
Lemma steps_app1
 :  forall n x1 x1' x2
 ,  STEPS n x1 x1'
 -> STEPS n (XApp x1 x2) (XApp x1' x2).
Proof.
 intros. induction H; eauto.
Qed.
Hint Resolve steps_app1.


Lemma steps_app2
 :  forall n v1 x2 x2'
 ,  value v1 
 -> STEPS n x2 x2'
 -> STEPS n (XApp v1 x2) (XApp v1 x2').
Proof.
 intros. induction H0; eauto.
Qed.
Hint Resolve steps_app2.



Require Export Exp.
Require Export SubstValueValue.


(* Single Small Step Evaluation ************************************)
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


(* Multi-step evaluation *******************************************)
Inductive STEPS : nat -> exp -> exp -> Prop :=
 | ESDone
   :  forall x1
   ,  STEPS 0 x1 x1

 | ESStep
   :  forall x1 x2
   ,  STEP  x1 x2
   -> STEPS 1 x1 x2

 | ESLink
   :  forall n1 n2 x1 x2 x3
   ,  STEPS n1 x1 x2 -> STEPS n2 x2 x3
   -> STEPS (n1 + n2) x1 x3.

Hint Constructors STEPS.


Lemma steps_app1
 :  forall n x1 x1' x2
 ,  STEPS n x1 x1'
 -> STEPS n (XApp x1 x2) (XApp x1' x2).
Proof.
 intros. induction H; eauto.
Qed.


Lemma steps_app2
 :  forall n v1 x2 x2'
 ,  value v1 
 -> STEPS n x2 x2'
 -> STEPS n (XApp v1 x2) (XApp v1 x2').
Proof.
 intros. induction H0; eauto.
Qed.


Require Export SubstValueValue.
Require Import Preservation.
Require Import TyJudge.
Require Export EsJudge.
Require Export Exp.


(* Multi-step evaluation ********************************************)
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


(* When we multi-step evaluate some expression,
   then the result has the same type as the original.
 *)  
Lemma preservation_steps
 :  forall n x1 t1 x2
 ,  TYPE Empty x1 t1
 -> STEPS n    x1 x2
 -> TYPE Empty x2 t1.
Proof.
 intros. 
 induction H0; eauto.
  eapply preservation; eauto.
Qed.


(* Big Step Evaluation **********************************************)
Inductive EVAL : exp -> exp -> Prop :=
 | EvValue
   :  forall v2
   ,  value v2
   -> EVAL v2 v2

 | EvLamApp
   :  forall x1 t11 x12 x2 v2 v3
   ,  value v2
   -> value v3
   -> EVAL x1 (XLam t11 x12)
   -> EVAL x2 v2
   -> EVAL (subst v2 x12) v3
   -> EVAL (XApp x1 x2) v3.

Hint Constructors EVAL.


Lemma steps_app1
 :  forall n x1 x1' x2
 ,  STEPS n x1 x1'
 -> STEPS n (XApp x1 x2) (XApp x1' x2).
Proof.
 intros. 
 induction H; eauto.
Qed.


Lemma steps_app2
 :  forall n v1 x2 x2'
 ,  value v1 
 -> STEPS n x2 x2'
 -> STEPS n (XApp v1 x2) (XApp v1 x2').
Proof.
 intros.
 induction H0; eauto.
Qed.


Lemma eval_to_steps
 :  forall x1 t1 v2
 ,  TYPE Empty x1 t1
 -> EVAL x1 v2
 -> (exists n,  STEPS n x1 v2).
Proof.
 intros x1 t1 v2 HT HE. gen t1.
 induction HE.
 Case "EVValue".
  intros.
  exists O. apply ESDone.

 Case "EVLamApp".
  intros. inverts HT.

  lets E1: IHHE1 H4. clear IHHE1. destruct E1 as [n1].
  lets E2: IHHE2 H6. clear IHHE2. destruct E2 as [n2].

  lets T1: preservation_steps H4 H1. 
  lets T2: preservation_steps H6 H2.
   inverts keep T1.
   lets T3: subst_value_value H7 T2.

  lets E3: IHHE3 T3. clear IHHE3. destruct E3 as [n3]. sort.

  exists (n1 + (n2 + (1 + n3))).
  eapply ESLink.
   eapply steps_app1. eauto.
  eapply ESLink.
   eapply steps_app2.
    lets C1: type_check_empty_tyenv_is_closed T1. auto.
   eauto.
  eapply ESLink.
   eapply ESStep. eapply ESLamApp.
    auto. 
  auto.
Qed.

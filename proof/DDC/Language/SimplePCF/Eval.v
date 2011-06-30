
Require Import DDC.Language.SimplePCF.Preservation.
Require Export DDC.Language.SimplePCF.SubstExpExp.
Require Export DDC.Language.SimplePCF.Step.
Require Export DDC.Language.SimplePCF.Ty.
Require Export DDC.Language.SimplePCF.Exp.


(********************************************************************)
(* Big Step Evaluation.
   This is also called 'Natural Semantics'.
   It provides a relation between the expression to be reduced 
   and its final value. 
 *)
Inductive EVAL : exp -> exp -> Prop :=

 (* Values are already evaluated ******)
 | EVDone
   :  forall v2
   ,  wnfX v2
   -> EVAL v2 v2

 (* Function Applications *************)
 | EVLamApp
   :  forall x1 t11 x12 x2 v2 v3
   ,  EVAL x1 (XLam t11 x12) -> EVAL x2 v2 -> EVAL (substX 0 v2 x12) v3
   -> EVAL (XApp x1 x2) v3

 (* Fixpoint recursion ****************)
 | EVFix 
   :  forall t11 x12 v3
   ,  EVAL (substX 0 (XFix t11 x12) x12) v3
   -> EVAL (XFix t11 x12) v3

 (* Naturals **************************)
 | EVSucc
   :  forall x1 n
   ,  EVAL x1 (XNat n)
   -> EVAL (XSucc x1) (XNat (S n))

 | EVPredZero
   :  forall x1
   ,  EVAL x1 (XNat O) 
   -> EVAL (XPred x1) (XNat O)

 | EVPredSucc
   :  forall x1 n
   ,  EVAL x1 (XNat (S n))
   -> EVAL (XPred x1) (XNat n)

 (* Booleans **************************)
 | EVIsZeroTrue
   :  forall x1
   ,  EVAL x1 (XNat O) 
   -> EVAL (XIsZero x1) XTrue

 | EVIsZeroFalse
   :  forall x1 n
   ,  EVAL x1 (XNat (S n))
   -> EVAL (XIsZero x1) XFalse

 (* Branching *************************)
 | EVIfThen
   :  forall x1 x2 x3 v2
   ,  EVAL x1 XTrue -> EVAL x2 v2
   -> EVAL (XIf x1 x2 x3) v2

 | EVIfElse
   :  forall x1 x2 x3 v3
   ,  EVAL x1 XFalse -> EVAL x3 v3
   -> EVAL (XIf x1 x2 x3) v3.

Hint Constructors EVAL.


(* A terminating big-step evaluation always produces a whnf.
   The fact that the evaluation terminated is implied by the fact
   that we have a finite proof of EVAL to pass to this lemma. *)
Lemma eval_produces_wnfX
 :  forall x1 v1
 ,  EVAL   x1 v1
 -> wnfX  v1.
Proof.
 intros. induction H; eauto.
Qed.
Hint Resolve eval_produces_wnfX.


(********************************************************************)
(* Big to Small steps.
   Convert a big-step evaluation into a list of individual
   machine steps. *)
Lemma steps_of_eval
 :  forall x1 t1 x2
 ,  TYPE nil x1 t1
 -> EVAL  x1 x2
 -> STEPS x1 x2.
Proof.
 intros x1 t1 v2 HT HE.
 gen t1.
 induction HE;
  intros; 
  inverts keep HT; eauto;

  (* This gets simple expressions with a single context *)
  try (  eapply EsAppend ;
       [ eapply steps_context ; [ eauto | eapply IHHE ; eauto ]
       | eauto ]).

 Case "EvLamApp".
  lets E1: IHHE1 H2. 
  lets E2: IHHE2 H4.
  lets T1: preservation_steps H2 E1. inverts keep T1.
  lets T2: preservation_steps H4 E2.
  lets T3: subst_exp_exp H1 T2.
  lets E3: IHHE3 T3.
  eapply EsAppend.
   lets D: steps_context XcApp1. eapply D. eauto.
  eapply EsAppend.
   eapply steps_context; eauto. 
  eapply EsAppend.
   eapply EsStep.
     eapply EsLamApp. eauto. eauto.

 Case "EvFix".
  lets T1: subst_exp_exp H3 HT.
  lets E1: IHHE T1.
  eapply EsAppend.
   eapply EsStep.
    eapply EsFix. eauto.

 Case "EVIfTrue".
  lets S1: IHHE1 H3.
  lets S2: IHHE2 H5.
  eapply EsAppend.
   eapply (steps_context (fun xx => XIf xx x2 x3)); eauto.
   eauto.

 Case "EVIfFalse".
  lets S1: IHHE1 H3.
  lets S2: IHHE2 H6.
  eapply EsAppend.
   eapply (steps_context (fun xx => XIf xx x2 x3)); eauto.
   eauto.
Qed.


(********************************************************************)
(* Small to Big steps.
   Convert a list of individual machine steps to a big-step
   evaluation. The main part of this is the expansion lemma, which 
   we use to build up the overall big-step evaluation one small-step
   at a time. The other lemmas are used to feed it small-steps.
 *)

(* Given an existing big-step evalution, we can produce a new one
   that does an extra step before returning the original value.
 *)
Lemma eval_expansion
 :  forall te x1 t1 x2 v3
 ,  TYPE te x1 t1
 -> STEP x1 x2 -> EVAL x2 v3 
 -> EVAL x1 v3.
Proof.
 intros te x1 t1 x2 v3 HT HS. gen te t1 v3.
 induction HS; intros;
  try (solve [inverts H; eauto]);
  try eauto.

 Case "Context".
  destruct H.
   eauto.
   inverts HT; inverts H0; try (inverts H); eauto.
   inverts HT; inverts H0; try (inverts H1); eauto.
   inverts HT; inverts H0; try (inverts H); eauto.
   inverts HT; inverts H0; try (inverts H); eauto.
   inverts HT; inverts H0; try (inverts H); eauto.
   inverts HT; inverts H0; try (inverts H); eauto.
Qed.


(* Convert a list of small steps to a big-step evaluation. *)
Lemma eval_of_stepsl
 :  forall x1 t1 v2
 ,  TYPE nil x1 t1
 -> STEPSL x1 v2 -> value v2
 -> EVAL   x1 v2.
Proof.
 intros.
 induction H0.
 
 Case "ESLNone".
   apply EVDone. inverts H1. auto.

 Case "ESLCons".
  eapply eval_expansion. 
   eauto. eauto. 
   apply IHSTEPSL.
   eapply preservation. eauto. auto. auto.
Qed.


(* Convert a multi-step evaluation to a big-step evaluation.
   We use stepsl_of_steps to flatten out the append constructors
   in the multi-step evaluation, leaving a list of individual
   small-steps.
 *)
Lemma eval_of_steps
 :  forall x1 t1 v2
 ,  TYPE nil x1 t1
 -> STEPS x1 v2 -> value v2
 -> EVAL  x1 v2.
Proof.
 intros.
 eapply eval_of_stepsl; eauto.
 apply  stepsl_of_steps; auto.
Qed.



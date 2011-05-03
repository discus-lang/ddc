
Require Export SubstValueValue.
Require Import Preservation.
Require Import TyJudge.
Require Export EsJudge.
Require Export Exp.


(* Big Step Evaluation **********************************************
   This is also called 'Natural Semantics'.
 *)
Inductive EVAL : exp -> exp -> Prop :=
 | EVValue
   :  forall v2
   ,  value v2
   -> EVAL v2 v2

 | EVLamApp
   :  forall x1 t11 x12 x2 v2 v3
   ,                              value v2               -> value v3
   -> EVAL x1 (XLam t11 x12) -> EVAL x2 v2 -> EVAL (subst v2 x12) v3
   -> EVAL (XApp x1 x2) v3.

Hint Constructors EVAL.


(* A terminating big-step evaluation always produces a value.
   The fact that the evaluation has terminated is implied by the fact
   that we have a proof of EVAL to pass to this lemma. If the
   evaluation was non-terminating, then we wouldn't have a finite
   proof of EVAL.
 *)
Lemma eval_produces_value
 :  forall x1 v1
 ,  EVAL x1 v1
 -> value v1.
Proof.
 intros. destruct H; auto.
Qed.
Hint Resolve eval_produces_value.


(* Big to Small steps ***********************************************)
Lemma eval_to_steps
 :  forall x1 t1 x2
 ,  TYPE Empty x1 t1
 -> EVAL x1 x2
 -> (exists n,  STEPS n x1 x2).
Proof.
 intros x1 t1 v2 HT HE. gen t1.
 induction HE.
 Case "EVValue".
  intros.
  exists O.
  apply ESDone.

 Case "EVLamApp".
  intros. inverts HT.

  lets E1: IHHE1 H4. clear IHHE1. destruct E1 as [n1].
  lets E2: IHHE2 H6. clear IHHE2. destruct E2 as [n2].

  lets T1: preservation_steps H4 H1. inverts keep T1.
  lets T2: preservation_steps H6 H2.
  lets T3: subst_value_value H7 T2.
  lets E3: IHHE3 T3. clear IHHE3. destruct E3 as [n3].

  exists (n1 + (n2 + (1 + n3))).
   repeat (try (eapply ESLink); eauto).
Qed.

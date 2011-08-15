
Require Import DDC.Language.SystemF2.Step.
Require Import DDC.Language.SystemF2.SubstExpExp.
Require Import DDC.Language.SystemF2.SubstTypeExp.
Require Import DDC.Language.SystemF2.SubstTypeType.
Require Import DDC.Language.SystemF2.TyJudge.


(* When an expression takes a step the result has the same type. *)
Theorem preservation
 :  forall ke te x x' t
 ,  TYPE ke te x t
 -> STEP x x'
 -> TYPE ke te x' t.
Proof.
 intros ke te x x' t HT HS.
 gen ke te x' t.
 induction x; intros.

 Case "XVar".
  inverts HS.

 Case "XLAM".
  inverts HS.

 Case "XAPP".
  inverts HT.
  inverts keep HS.

  SCase "ESAPPLAM".
   inverts H3.
   eapply subst_type_exp in H4; eauto.
   rewrite substTE_liftTE in H4. auto.

  SCase "ESAPP1".
   apply TYAPP; eauto.

 Case "XLam".
  inverts HS.

 Case "XApp".
  inverts HT.
  inverts keep HS.
  
  SCase "ESAppLam".
   inverts H3.
   lets D: subst_exp_exp H9 H5. auto.

  SCase "EsApp1".
   eapply TYApp; eauto.

  SCase "EsApp2".
   eapply TYApp; eauto.
Qed.
Hint Resolve preservation.


(* When we multi-step evaluate some expression,
   then the result has the same type as the original. *)
Lemma preservation_steps
 :  forall x1 t1 x2
 ,  TYPE nil nil x1 t1
 -> STEPS x1 x2
 -> TYPE nil nil x2 t1.
Proof.
 intros. induction H0; burn.
Qed.
Hint Resolve preservation_steps.


(* When we multi-step evaluate some expression, 
   then the result has the same type as the original.
   Using the left-linearised form for the evaluation. *)
Lemma preservation_stepsl
 :  forall x1 t1 x2
 ,  TYPE nil nil x1 t1
 -> STEPSL x1 x2
 -> TYPE nil nil x2 t1.
Proof.
 intros. induction H0; burn.
Qed.
Hint Resolve preservation_stepsl.


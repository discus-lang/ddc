
Require Import DDC.Language.SystemF2Data.Step.
Require Import DDC.Language.SystemF2Data.TyJudge.
Require Import DDC.Language.SystemF2Data.SubstExpExp.
Require Import DDC.Language.SystemF2Data.SubstTypeExp.

(* When a well typed expression transitions to the next state
   then its type is preserved. *)
Theorem preservation
 :  forall ds x x' t
 ,  TYPE ds nil nil x  t
 -> STEP x x'
 -> TYPE ds nil nil x' t.
Proof.
 intros ds x x' t HT HS. gen t.
 induction HS; intros; inverts_type; eauto.

 (* Evaluation in an arbitrary context. *)
 Case "EsContext".
  destruct H; inverts_type; eauto. 

  SCase "XCon".
   eapply TYCon; eauto.
   eapply exps_ctx_Forall2_swap; eauto.

 Case "EsLamApp".
  eapply subst_exp_exp; eauto.

 Case "EsLAMAPP".
  assert (TYPE ds nil (substTE 0 t2 nil) (substTX 0 t2 x12) (substTT 0 t2 t1)) as HT
      by (eapply subst_type_exp; eauto).
  simpl in HT. auto.

 Case "EsCaseAlt".
  eapply subst_exp_exp_list; eauto.
  have (In (AAlt dc x) alts).

  nforall. (* todo: burn should get this *)
  have (TYPEA ds nil nil (AAlt dc x) (makeTApps (TCon tc) ts) t) as HA.
  inverts HA.
  rewrite H11 in H16. inverts H16.
  rewrite H15 in H10. inverts H10.

  have (getCtorOfType (TCon tc0) = Some tc0) as HTC.
   erewrite getCtorOfType_makeTApps in H5; eauto.
   inverts H5.
  rewrite H6 in H15. inverts H15.
  rr.
  have (length ts = length ks0)      as HTK1.
  have (length tsParam = length ks0) as HTK2.
  rewrite <- HTK1 in HTK2.
  assert (tsParam = ts).
   eapply makeTApps_args_eq; eauto. 
   subst.
  eauto.
Qed.


(* When we multi-step evaluate some expression,
   then the result has the same type as the original. *)  
Lemma preservation_steps
 :  forall ds x1 t1 x2
 ,  TYPE ds nil nil x1 t1
 -> STEPS       x1 x2
 -> TYPE ds nil nil x2 t1.
Proof.
 intros ds x1 t1 x2 HT HS.
 induction HS; eauto using preservation.
Qed.


(* When we multi-step evaluate some expression, 
   then the result has the same type as the original.
   Using the left-linearised form for the evaluation.
 *)
Lemma preservation_stepsl
 :  forall ds x1 t1 x2
 ,  TYPE ds nil nil x1 t1
 -> STEPSL x1 x2
 -> TYPE ds nil nil x2 t1.
Proof.
 intros ds x1 t1 x2 HT HSL.
 induction HSL; eauto using preservation.
Qed.


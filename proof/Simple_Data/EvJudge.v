
Require Export SubstExpExp.
Require Import Preservation.
Require Import TyJudge.
Require Export EsJudge.
Require Export Exp.


(* Big Step Evaluation **********************************************
   This is also called 'Natural Semantics'.
   It provides a relation between the expression to be reduced 
   and its final value. 
 *)
Inductive EVAL : exp -> exp -> Prop :=
 | EvDone
   :  forall v2
   ,  whnfX  v2
   -> EVAL   v2 v2

 | EvLamApp
   :  forall x1 t11 x12 x2 v2 v3
   ,  EVAL x1 (XLam t11 x12)
   -> EVAL x2 v2
   -> EVAL (substX 0 v2 x12) v3
   -> EVAL (XApp x1 x2)      v3

 | EvCon
   :  forall dc xs vs v3
   ,  Forall2 EVAL xs vs
   -> Forall whnfX vs
   -> EVAL (XCon dc vs) v3
   -> EVAL (XCon dc xs) v3

 | EvCase 
   :  forall x1 x2 v3 dc vs alts tsArgs
   ,  EVAL x1 (XCon dc vs)
   -> Forall whnfX vs
   -> getAlt dc alts = Some (AAlt dc tsArgs x2)
   -> EVAL (substXs 0 vs x2) v3
   -> EVAL (XCase x1 alts)   v3.

Hint Constructors EVAL.


(* A terminating big-step evaluation always produces a whnf.
   The fact that the evaluation terminated is implied by the fact
   that we have a finite proof of EVAL to pass to this lemma. *)
Lemma eval_produces_whnfX
 :  forall x1 v1
 ,  EVAL   x1 v1
 -> whnfX  v1.
Proof.
 intros. induction H; intros; eauto.
Qed.
Hint Resolve eval_produces_whnfX.


(* Big to Small steps ***********************************************
   Convert a big-step evaluation into a list of individual
   machine steps.
 *)

Lemma steps_of_evals
 :  forall ds xs ts vs 
 ,  Forall2 (TYPE ds Empty) xs ts
 -> Forall2 EVAL  xs vs
 -> Forall2 STEPS xs vs.
Proof.
 admit.
Qed.

Lemma steps_con
 :  forall ix x v vs xs xs' dc
 ,  Forall value vs
 -> STEPS x v
 -> splitAt ix xs = (vs, x :: xs')
 -> STEPS (XCon dc xs) (XCon dc (vs ++ (v :: xs'))).
Proof.
 intros.
 lets D: steps_context XcCon. eapply (XscIx ix). eauto. auto.
 lets D1: D H0. clear D.
  assert (xs = app vs (x :: xs')). skip.
   rewrite H2.
 apply D1.
Qed.


Lemma steps_con3
 :  forall ix vs xs xs' vs' dc x v acc
 ,  Forall  value vs 
 -> STEPS   x v             -> value v
 -> Forall2 STEPS xs' vs'   -> Forall  value vs'
 -> splitAt ix acc xs = Some (app acc vs, x :: xs')
 -> STEPS (XCon dc xs) (XCon dc (vs ++ (v :: vs'))).
Proof.
 intros. gen x v xs xs' vs vs' acc.
 induction ix; intros.

 Case "ix = 0".
  admit.

 Case "ix = S ix'".
  assert (xs = app vs (x :: xs')). admit. (* prop splitAt *)
  rewrite H5.

  (* do the first eval *)
  eapply EsAppend.
   lets D: steps_context XcCon. 
    eapply XscIx with (ix := List.length vs)
                      (vs := vs).
    skip. 
    auto.
   lets D1: D H0. eapply D1.

  (* do the rest, need to reassociate ++ *)
  destruct vs' as [vs2 | v2].
   SCase "vs' = nil".
    inverts H2. auto.

   SCase "vs' = vs2 :: v2".
    assert (app vs (v :: v2 :: vs') = app (app vs (v :: nil)) (v2::vs')).
     admit.
    rewrite H6.

   inverts H2.
   eapply IHix.
    eauto.
    inverts H3. eauto.
    admit. (* ok *)
    eauto.
    inverts H3. eauto. clear IHix.
    rewrite H5 in H4.

    assert ( splitAt ix nil (app vs (v :: x0 :: l0))
           = Some (app nil (app vs (v :: nil)), x0 :: l0)).
     simpl.

    (* we've gone the wrong way wirh the rewrite *)
    (* almosted worked though.
        splitAt ix acc (acc ++ xx, yy) => ix == length ix
       this is all the info we should need from the hyp 
      
       do more lemmas involving splitAt
       maybe better to use:
       splitAt ix xx = (take ix xx, drop ix xx)

       .. or not, because we don't want ix in the right
       of the definition. Just do this as a lemma.
     *)

    admit.
    eapply H2.

 
 (********* junk *)
  admit.

  assert (xs = app vs xs'). admit. rewrite H3. clear H3.
 
  destruct xs'.
   admit.
   eapply EsAppend.
    lets D: steps_context XcCon. eapply XscIx. eauto. eauto.
    destruct vs'.
     inverts H0.
     inverts H0.
      eapply D. eauto.


   assert (xs = app vs (e :: xs')). admit. (* ok, prop of splitAt *)
   eapply EsAppend.
   inverts H0.
    eapply steps_con4 
      with (xs' := xs') (ix  := S ix) (x := e). eauto. eauto.
    
     rewrite <- H3. 
      assert (app nil vs = vs). simpl. auto.
      rewrite <- H0 in H2. eauto.
      simpl. 
      rewrite <- H3. clear H3.

      destruct xs'.
       admit.
       assert (splitAt ix nil xs = Some (app vs (e :: nil), e0:: xs')).
        admit. clear H2.

      destruct vs'  as [vs2 | v0]. inverts H0.
      destruct vs'  as [vs3 | v1]. inverts H0. inverts H8.
     
      assert (xs = app vs (e :: e0 :: xs')).
       admit. rewrite H2.

     eapply EsAppend.
     eapply IHix.
      eauto. rewrite <- H2. eauto.

      

Lemma steps_con
 : forall xs vs dc 
 ,  Forall2 STEPS xs vs 
 -> Forall  value vs
 -> STEPS (XCon dc xs) (XCon dc vs).
Proof.
 intros.
 induction vs.
  inverts H. auto.

  destruct xs. 
   inverts H.

   assert (a :: vs = app nil (a :: vs)). 
    simpl. auto. rewrite H1. clear H1.
   eapply (steps_con3 0 e a) with (xs' := xs). simpl. auto.
   auto.
   inverts H. auto.
   inverts H. auto.
Qed.
 

Lemma steps_of_eval
 :  forall ds x1 t1 x2
 ,  TYPE ds Empty x1 t1
 -> EVAL  x1 x2
 -> STEPS x1 x2.
Proof.
 intros ds x1 t1 v2 HT HE. gen t1.

 (* Induction over the form of (EVAL x1 x2) *)
 induction HE.
 Case "EvDone".
  intros. apply EsNone.

 Case "EvLamApp".
  intros. inverts HT.

  lets E1: IHHE1 H3. 
  lets E2: IHHE2 H5.

  lets T1: preservation_steps H3 E1. inverts keep T1.
  lets T2: preservation_steps H5 E2.
  lets T3: subst_value_value H2 T2.
  lets E3: IHHE3 T3.

  eapply EsAppend.
    lets D: steps_context XcApp1. eapply D. eauto. 
   eapply EsAppend.
    lets D: steps_context (XcApp2 (XLam t0 x12)). eauto.
    eapply D. eauto.
   eapply EsAppend.
    eapply EsStep.
     eapply EsLamApp. eauto. eauto.

 Case "EvCon".
  intros. inverts keep HT.
  lets D: steps_of_evals H. eauto.
   eapply steps_con. eauto.
    admit. (* ok, vs values *)
    eapply IHHE. eauto.
    eapply TYCon. eauto.
    admit. (* ok, vs well typed *)

  

 Case "EvCase".
  intros. inverts keep HT.

  lets Ex1: IHHE1 H3. clear IHHE1.

  eapply EsAppend.
   (* evaluate the discriminant *)
   lets HSx1: steps_context XcCase. eapply HSx1.
    eapply Ex1.

  (* choose the alternative *)
  lets HTCon: preservation_steps H3 Ex1. clear Ex1.
  inverts HTCon.
  assert (tsArgs0 = tsArgs).
   eapply getAlt_matches_dataDef; eauto. subst.

  lets HA: getAltExp_hasAlt H0.
  rewrite Forall_forall in H4.
  apply H4 in HA. clear H4.
  inverts HA.

   (* substitute ctor values into alternative *)
  eapply EsAppend.
   eapply EsStep.
    eapply EsCaseAlt.
     assert (Forall closedX vs).
     admit.
     rewrite Forall_forall.
     intros.
     apply Value.
     rewrite Forall_forall in H. eauto.
     rewrite Forall_forall in H1. eauto.
     eauto.
     eapply IHHE2.
     eapply subst_value_value_list; eauto.
Qed.


(* Small to Big steps ***********************************************
   Convert a list of individual machine steps to a big-step
   evaluation. The main part of this is the expansion lemma, which 
   we use to build up the overall big-step evaluation one small-step
   at a time. The other lemmas are used to feed it small-steps.
 *)

(* Given an existing big-step evalution, we can produce a new one
   that does an extra step before returning the original value.
 *)
Lemma eval_expansion
 :  forall ds te x1 t1 x2 v3
 ,  TYPE ds te x1 t1
 -> STEP x1 x2 -> EVAL x2 v3 
 -> EVAL x1 v3.
Proof.
 intros ds te x1 t1 x2 v3 HT HS. gen ds te t1 v3.
 induction HS; intros; 
  try (solve [inverts H; eauto]);
  try eauto.

 Case "Context".
  destruct H.
   eauto.

   SCase "XcApp1".
    inverts HT. inverts H0. inverts H. eauto.

   SCase "XcApp2".
    inverts HT. inverts H0. inverts H1. eauto.

   SCase "XcCon".
    inverts HT. inverts H0. inverts H1.
    admit. (********* TODO: need big step rule *)

   SCase "XcCase".
    inverts HT. inverts H0. inverts H. eauto.
    eapply EvCase with (dc := dc) (vs := vs).
     admit. (*** ok, vs are values. need big step rule for XCon *)
    rewrite Forall_forall. intros.
    rewrite Forall_forall in H.
    apply H in H2. inverts H2. eauto. eauto.
    auto.
Qed.


(* Convert a list of small steps to a big-step evaluation. *)
Lemma eval_of_stepsl
 :  forall ds x1 t1 v2
 ,  TYPE ds Empty x1 t1
 -> STEPSL x1 v2 -> value v2
 -> EVAL   x1 v2.
Proof.
 intros.
 induction H0.
 
 Case "EslNone".
   apply EvDone. inverts H1. auto.

 Case "EslCons".
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
 :  forall ds x1 t1 v2
 ,  TYPE ds Empty x1 t1
 -> STEPS x1 v2 -> value v2
 -> EVAL  x1 v2.
Proof.
 intros.
 eapply eval_of_stepsl; eauto.
 apply  stepsl_of_steps; auto.
Qed.



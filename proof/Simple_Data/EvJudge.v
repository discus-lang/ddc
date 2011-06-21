
Require Export SubstExpExp.
Require Import Preservation.
Require Import TyJudge.
Require Export EsJudge.
Require Export Exp.
Require Import BaseList.


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
   :  forall dc xs vs
   ,  EVALS xs vs
   -> EVAL  (XCon dc xs) (XCon dc vs)

 | EvCase 
   :  forall x1 x2 v3 dc vs alts tsArgs
   ,  EVAL x1 (XCon dc vs)
   -> Forall whnfX vs
   -> getAlt dc alts = Some (AAlt dc tsArgs x2)
   -> EVAL (substXs 0 vs x2) v3
   -> EVAL (XCase x1 alts)   v3

 with EVALS : list exp -> list exp -> Prop :=
  | EvsNil
    :  EVALS nil nil

  | EvsHead
    :  forall x v xs vs
    ,  EVAL x v
    -> EVALS xs vs 
    -> EVALS (x :: xs) (v :: vs)

  | EvsCons
    :  forall v xs vs
    ,  whnfX v
    -> EVALS xs vs
    -> EVALS (v :: xs) (v :: vs).

Hint Constructors EVAL.
Hint Constructors EVALS.


Theorem EVAL_mutind
 :  forall (PE : exp      -> exp      -> Prop)
           (PS : list exp -> list exp -> Prop)

 ,  (forall v2 
    ,  whnfX v2                   
    -> PE v2 v2) 
 -> (forall x1 t11 x12 x2 v2 v3
    ,  EVAL x1 (XLam t11 x12)    -> PE x1 (XLam t11 x12) 
    -> EVAL x2 v2                -> PE x2 v2 
    -> EVAL (substX 0 v2 x12) v3 -> PE (substX 0 v2 x12) v3
    -> PE (XApp x1 x2) v3) 
 -> (forall dc xs vs
    ,  EVALS xs vs               -> PS xs vs
    -> PE (XCon dc xs) (XCon dc vs)) 
 -> (forall x1 x2 v3 dc vs alts tsArgs
    ,  EVAL x1 (XCon dc vs)      -> PE x1 (XCon dc vs) 
    -> Forall whnfX vs 
    -> getAlt dc alts = Some (AAlt dc tsArgs x2) 
    -> EVAL (substXs 0 vs x2) v3 -> PE (substXs 0 vs x2) v3 
    -> PE (XCase x1 alts) v3) 

 -> (  PS nil nil)
 -> (forall x v xs vs
    ,  EVAL x v                  -> PE x  v
    -> EVALS xs vs               -> PS xs vs
    -> PS (x :: xs) (v :: vs))
 -> (forall v xs vs
    ,  whnfX v
    -> EVALS xs vs               -> PS xs vs
    -> PS (v :: xs) (v :: vs))
 -> forall x1 x2
 ,  EVAL x1 x2 -> PE x1 x2.

Proof.
 intros PE PS.
 intros Hdone Hlam Hcon Hcase Hnil Hhead Hcons.
 refine (fix  IHPE x  x'  (HE: EVAL  x  x')  {struct HE} 
              : PE x x'   := _
         with IHPS xs xs' (HS: EVALS xs xs') {struct HS}
              : PS xs xs' := _
         for  IHPE).

 case HE; intros.
 eapply Hdone; eauto.
 eapply Hlam;  eauto.
 eapply Hcon;  eauto.
 eapply Hcase; eauto.

 case HS; intros.
 eapply Hnil.
 eapply Hhead; eauto.
 eapply Hcons; eauto.
Qed.


(* A terminating big-step evaluation always produces a whnf.
   The fact that the evaluation terminated is implied by the fact
   that we have a finite proof of EVAL to pass to this lemma. *)
Lemma eval_produces_whnfX
 :  forall x1 v1
 ,  EVAL   x1 v1
 -> whnfX  v1.
Proof.
 intros.
 induction H using EVAL_mutind with
  (PS := fun xs vs 
      => EVALS xs vs 
      -> Forall whnfX vs);
  intros; eauto.
Qed.
Hint Resolve eval_produces_whnfX.


Lemma evals_produces_whnfX
 :  forall xs vs
 ,  EVALS xs vs
 -> Forall whnfX vs.
Proof.
 intros.
 induction H; eauto.
Qed.
Hint Resolve evals_produces_whnfX.


Inductive CHAIN : list exp -> list exp -> Prop :=
 | EcDone
   :  forall vs
   ,  Forall whnfX vs
   -> CHAIN vs vs

 | EcCons
   :  forall x v vs C
   ,  exps_ctx C  -> STEPS x v
   -> CHAIN (C v) vs
   -> CHAIN (C x) vs.

Hint Constructors CHAIN.


Lemma make_chain
 :  forall xs vs
 ,  Forall2 STEPS xs vs
 -> Forall  whnfX vs
 -> CHAIN xs vs.
Proof.
 intros. gen vs.
  induction xs as [xs | x]; intros.
  inverts H. auto.
 
  destruct vs as [vs | v].
   inverts H.

  inverts H. inverts H0.
  assert (CHAIN xs vs). auto.
   clear IHxs.

  (* TODO: this comes from STEPS xs vs *)
  assert (Forall2 (fun x v => whnfX x \/ STEPS x v) xs vs).
   admit. (* ok, STEPS xs vs *)

  (* either all the xs are already whnfX,
      or there is a context where one can step *)
  lets D: exps_ctx_run_Forall2 H0. clear H0.
  inverts D.

  Case "all whnfX".
   assert (xs = vs). admit. subst. (* ok, stepping whnf yields same *)
   assert (x = v).   admit. subst. (* ok, stepping whnf yields same *)
   eapply EcDone. auto.

  Case "something steps".
   destruct H0 as [C].
   destruct H0 as [x'].
   destruct H0 as [v'].
   inverts H0. inverts H5. inverts H7.

   lets C1: XscNil (C x').  (* context for reduction of first elem *)
   lets C2: XscCons H2 H0.  (* context for reduction in tail *)

   (* final elem of chain, all elems whnfX *)
   assert (CHAIN (v :: C v') (v :: C v')).
    auto.

   lets D1: EcCons x' v' (v :: C v') C2 H5. auto. (* reduction in tail *)
   lets D2: EcCons x  v  (v :: C v') C1 D1. auto. (* reduction in head *)
   auto.
Qed.


Lemma steps_chain_XCon
 :  forall xs vs dc
 ,  CHAIN xs vs
 -> STEPS (XCon dc xs) (XCon dc vs).
Proof.
 intros.
 induction H.
  eauto.
  eapply (EsAppend (XCon dc (C x)) (XCon dc (C v))).
   eapply steps_context_XCon.
     auto. auto.
   auto.
Qed.


Lemma steps_in_XCon
 :  forall xs vs dc
 ,  Forall2 STEPS xs vs
 -> Forall whnfX vs
 -> STEPS (XCon dc xs) (XCon dc vs).
Proof.
 intros.
 eapply steps_chain_XCon.
 eapply make_chain. auto. auto.
Qed.


(* Big to Small steps ***********************************************
   Convert a big-step evaluation into a list of individual
   machine steps.
 *)
Lemma steps_of_eval
 :  forall ds x1 t1 x2
 ,  TYPE ds Empty x1 t1
 -> EVAL  x1 x2
 -> STEPS x1 x2.
Proof.
 intros ds x1 t1 v2 HT HE. gen t1.
 induction HE using EVAL_mutind with
  (PS := fun xs vs => forall ts
      ,  Forall2 (TYPE ds Empty) xs ts
      -> EVALS xs vs
      -> Forall2 STEPS xs vs)
  ; intros.

 Case "EvDone".
  intros. apply EsNone.

 (* Function Application ***)
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

 (* Constructor evaluation ***)
 Case "EvCon".
  intros.
  inverts HT.
  lets D: IHHE H8 H.
  eapply steps_in_XCon; eauto.
 

 (* Case selection ***)  
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
     auto.
     rewrite Forall_forall in H. eauto.
     eapply IHHE2.
     eapply subst_value_value_list; eauto.

 Case "EvsNil".
 auto.

 Case "EvsHead".
  destruct ts. 
   inverts H0. 
   inverts H0. eauto.

 Case "EvsCons".
  destruct ts.
   inverts H1.
   inverts H1. eauto.
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
    inverts HT. 

    assert (exists t, TYPE ds te x t). 
    eapply context_Forall2_exists_left. 
     eauto. eauto. destruct H1.

   inverts H0.
    inverts H2.
    eapply EvCon. 
     clear H9.
     induction H; intros.
      inverts H5.
      eapply EvsHead. eauto.
       induction xs. eauto.
        inverts H6. eauto.
        inverts H5. eauto.

    eapply EvCon. 
     clear H3 H4 H9.
     gen vs.
     induction H; intros.
      inverts H8. eauto. eauto.
      inverts H8. eauto. eauto.
      
   SCase "XcCase".
    inverts HT. inverts H0. inverts H. eauto. 
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



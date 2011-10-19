
Require Import DDC.Language.SystemF2Data.Preservation.
Require Export DDC.Language.SystemF2Data.SubstExpExp.
Require Export DDC.Language.SystemF2Data.SubstTypeExp.
Require Export DDC.Language.SystemF2Data.Step.
Require Import DDC.Language.SystemF2Data.TyJudge.
Require Export DDC.Language.SystemF2Data.Exp.
Require Import DDC.Base.


(* Big Step Evaluation
   This is also called 'Natural Semantics'.
   It provides a relation between the expression to be reduced 
   and its final value. *)
Inductive EVAL : exp -> exp -> Prop :=
 | EvDone
   :  forall v2
   ,  wnfX  v2
   -> EVAL   v2 v2

 | EVLAMAPP
   : forall x1 x12 t2 v3
   ,  EVAL x1 (XLAM x12)
   -> EVAL (substTX 0 t2 x12) v3
   -> EVAL (XAPP x1 t2) v3

 | EvLamApp
   :  forall x1 t11 x12 x2 v2 v3
   ,  EVAL x1 (XLam t11 x12)
   -> EVAL x2 v2
   -> EVAL (substXX 0 v2 x12) v3
   -> EVAL (XApp x1 x2)      v3

 | EvCon
   :  forall dc ts xs vs
   ,  EVALS xs vs
   -> EVAL  (XCon dc ts xs) (XCon dc ts vs)

 | EvCase 
   :  forall x1 x2 v3 dc ts vs alts
   ,  EVAL x1 (XCon dc ts vs)
   -> Forall wnfX vs
   -> getAlt dc alts = Some (AAlt dc x2)
   -> EVAL (substXXs 0 vs x2) v3
   -> EVAL (XCase x1 alts)   v3

 with EVALS : list exp -> list exp -> Prop :=
  | EvsNil
    :  EVALS nil nil

  | EvsCons
    :  forall x v xs vs
    ,  EVAL  x  v
    -> EVALS xs vs 
    -> EVALS (x :: xs) (v :: vs).

Hint Constructors EVAL.
Hint Constructors EVALS.


(* Invert all hypothesis that are compound eval statements. *)
Ltac inverts_eval :=
 repeat
  (match goal with 
   | [ H: EVAL (XApp _ _)    _ |- _ ] => inverts H
   | [ H: EVAL (XAPP _ _)    _ |- _ ] => inverts H
   | [ H: EVAL (XCon _ _ _)  _ |- _ ] => inverts H
   | [ H: EVAL (XCase _ _)   _ |- _ ] => inverts H
   end).


Theorem EVAL_mutind
 :  forall (PE : exp      -> exp      -> Prop)
           (PS : list exp -> list exp -> Prop)

 ,  (forall v2 
    ,  wnfX v2                   
    -> PE v2 v2) 
 -> (forall x1 x12 t2 v3
    ,  EVAL x1 (XLAM x12)         -> PE x1 (XLAM x12)
    -> EVAL (substTX 0 t2 x12) v3 -> PE (substTX 0 t2 x12) v3
    -> PE   (XAPP x1 t2) v3)
 -> (forall x1 t11 x12 x2 v2 v3
    ,  EVAL x1 (XLam t11 x12 )    -> PE x1 (XLam t11 x12) 
    -> EVAL x2 v2                 -> PE x2 v2 
    -> EVAL (substXX 0 v2 x12) v3 -> PE (substXX 0 v2 x12) v3
    -> PE (XApp x1 x2) v3) 
 -> (forall dc xs ts vs
    ,  EVALS xs vs                -> PS xs vs
    -> PE (XCon dc ts xs) (XCon dc ts vs)) 
 -> (forall x1 x2 v3 dc ts vs alts
    ,  EVAL x1 (XCon dc ts vs)       -> PE x1 (XCon dc ts vs) 
    -> Forall wnfX vs 
    -> getAlt dc alts = Some (AAlt dc x2) 
    -> EVAL (substXXs 0 vs x2) v3 -> PE (substXXs 0 vs x2) v3 
    -> PE (XCase x1 alts) v3) 

 -> (  PS nil nil)
 -> (forall x v xs vs
    ,  EVAL x v                   -> PE x  v
    -> EVALS xs vs                -> PS xs vs
    -> PS (x :: xs) (v :: vs))
 -> forall x1 x2
 ,  EVAL x1 x2 -> PE x1 x2.

Proof.
 intros PE PS.
 intros Hdone HLAM Hlam Hcon Hcase Hnil Hcons.
 refine (fix  IHPE x  x'  (HE: EVAL  x  x')  {struct HE} 
              : PE x x'   := _
         with IHPS xs xs' (HS: EVALS xs xs') {struct HS}
              : PS xs xs' := _
         for  IHPE).

 case HE; intros.
 eapply Hdone; eauto.
 eapply HLAM;  eauto.
 eapply Hlam;  eauto.
 eapply Hcon;  eauto.
 eapply Hcase; eauto.

 case HS; intros.
 eapply Hnil.
 eapply Hcons; eauto.
Qed.


(* A terminating big-step evaluation always produces a whnf.
   The fact that the evaluation terminated is implied by the fact
   that we have a finite proof of EVAL to pass to this lemma. *)
Lemma eval_produces_wnfX
 :  forall x1 v1
 ,  EVAL   x1 v1
 -> wnfX  v1.
Proof.
 intros.
 induction H using EVAL_mutind with
  (PS := fun xs vs 
      => EVALS xs vs 
      -> Forall wnfX vs);
  intros; eauto.
Qed.
Hint Resolve eval_produces_wnfX.


Lemma evals_produces_wnfX
 :  forall xs vs
 ,  EVALS xs vs
 -> Forall wnfX vs.
Proof.
 intros.
 induction H; eauto.
Qed.
Hint Resolve evals_produces_wnfX.


(* Big to Small steps
   Convert a big-step evaluation into a list of individual
   machine steps. *)
Lemma steps_of_eval
 :  forall ds x1 t1 x2
 ,  TYPE ds nil nil x1 t1
 -> EVAL  x1 x2
 -> STEPS x1 x2.
Proof.
 intros ds x1 t1 v2 HT HE. gen t1.
 induction HE using EVAL_mutind with
  (PS := fun xs vs => forall ts
      ,  Forall2 (TYPE ds nil nil) xs ts
      -> EVALS xs vs
      -> Forall2 STEPS xs vs)
  ; intros.

 Case "EvDone".
  intros. apply EsNone.

 (* Type Application ***)
 Case "EVLAMAPP".
  intros. inverts HT.
  lets E1: IHHE1 H1. clear IHHE1.
  lets T1: preservation_steps H1 E1. inverts keep T1.
  lets T2: subst_type_exp H2 H3.
   simpl in T2.
  lets E2: IHHE2 T2.
  eapply EsAppend.
   lets D: steps_context XcAPP. eapply D. eauto.
  eapply EsAppend.
   eapply EsStep.
    eapply EsLAMAPP. auto.


 (* Function Application ***)
 Case "EvLamApp".
  intros. inverts HT.

  lets E1: IHHE1 H1. 
  lets E2: IHHE2 H3.

  lets T1: preservation_steps H1 E1. inverts keep T1.
  lets T2: preservation_steps H3 E2.
  lets T3: subst_exp_exp H6 T2.
  lets E3: IHHE3 T3.

  eapply EsAppend.
    lets D: steps_context XcApp1. eapply D. eauto. 
   eapply EsAppend.
    lets D: steps_context (XcApp2 (XLam t0 x12)).
     eapply Value. eauto. unfold closedX.
     have    (0 = @length ki nil). rewrite H at 1.
     rrwrite (0 = @length ty nil). eauto.
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
  assert (TYPEA ds nil nil (AAlt dc x2) tObj t1) as TA.
   nforall. eauto. 

  inverts TA. inverts HTCon.
  rewrite H17 in H10. inverts H10.
  rewrite H16 in H9.  inverts H9.
  clear H7.
  assert (ts = tsParam).
   eapply makeTApps_args_eq; eauto.
   have (length ts = length ks0)       as HL1.
   have (length tsParam = length ks0)  as HL2.
   rewrite <- HL2 in HL1. eauto. 
   subst.

  (* substitute ctor values into alternative *)
  eapply EsAppend.
   eapply EsStep.
    eapply EsCaseAlt.
     auto.
     rewrite Forall_forall in H. eauto.
     
     eapply IHHE2.
     eapply subst_exp_exp_list; eauto.

 Case "EvsNil".
  auto.

 Case "EvsHead".
  destruct ts. 
   inverts H0. 
   inverts H0. eauto.
Qed.



(* Small to Big steps
   Convert a list of individual machine steps to a big-step
   evaluation. The main part of this is the expansion lemma, which 
   we use to build up the overall big-step evaluation one small-step
   at a time. The other lemmas are used to feed it small-steps. *)

(* Given an existing big-step evalution, we can produce a new one
   that does an extra step before returning the original value.
 *)
Lemma eval_expansion
 :  forall ds ke te x1 t1 x2 v3
 ,  TYPE ds ke te x1 t1
 -> STEP x1 x2 -> EVAL x2 v3 
 -> EVAL x1 v3.
Proof.
 intros ds ke te x1 t1 x2 v3 HT HS. gen ds ke te t1 v3.
 induction HS; intros; 
  try (solve [inverts H; eauto]);
  try eauto.

 Case "Context".
  destruct H; inverts_type; eauto.

   SCase "XcApp1".
    inverts_eval. inverts H. eauto.
   
   SCase "XcApp2".
    inverts_eval. inverts H1. eauto.

   SCase "XcAPP".
    inverts_eval. inverts H. eauto.

   SCase "XcCon".
    assert (exists t, TYPE ds ke te x t).
    eapply exps_ctx_Forall2_exists_left; eauto.
    dest t.
 
    inverts_eval.
    inverts H2.
    eapply EvCon. clear H9. 
 
    induction H; intros.
      inverts H3.
      eapply EvsCons. eauto.
       induction xs. eauto.
        inverts H7. eauto.
        inverts H3. eauto.

    eapply EvCon.
     clear H5 H6 H9. 
     gen vs.
     induction H; intros.
      inverts H11. eauto.
      inverts H11. eauto.
      
   SCase "XcCase".
    inverts H0. inverts H. eauto. 
Qed.


(* Convert a list of small steps to a big-step evaluation. *)
Lemma eval_of_stepsl
 :  forall ds x1 t1 v2
 ,  TYPE ds nil nil x1 t1
 -> STEPSL x1 v2 -> value v2
 -> EVAL   x1 v2.
Proof.
 intros.
 induction H0.
 
 Case "EslNone".
   apply EvDone. inverts H1. auto.

 Case "EslCons".
  eapply eval_expansion; eauto.
   apply IHSTEPSL; auto.
   eapply preservation; eauto.
Qed.


(* Convert a multi-step evaluation to a big-step evaluation.
   We use stepsl_of_steps to flatten out the append constructors
   in the multi-step evaluation, leaving a list of individual
   small-steps.
 *)
Lemma eval_of_steps
 :  forall ds x1 t1 v2
 ,  TYPE ds nil nil x1 t1
 -> STEPS x1 v2 -> value v2
 -> EVAL  x1 v2.
Proof.
 intros.
 eapply eval_of_stepsl; eauto.
 apply  stepsl_of_steps; auto.
Qed.




Require Export DDC.Language.SystemF2Store.Store.
Require Export DDC.Language.SystemF2Store.StepContext.
Require Export DDC.Language.SystemF2Store.TyJudge.
Require Export DDC.Language.SystemF2Store.SubstExpExp.
Require Export DDC.Language.SystemF2Store.Exp.

(********************************************************************)
(** * Single Small Step Evaluation *)
(** The single step rules model the individual transitions that the 
     machine can make at runtime. *)

Inductive STEP : store -> exp -> store -> exp -> Prop :=

 (* Step some sub-expression in an evaluation context *)
 | EsContext 
   :  forall C s x s' x'
   ,  exp_ctx C
   -> STEP s x      s' x'
   -> STEP s (C x)  s' (C x')

 | EsLamApp
   : forall s t11 x12 v2
   ,  wnfX v2
   -> STEP s (XApp   (XLam t11 x12) v2)
           s (substXX 0 v2 x12)

 (* Type application *)
 | EsLAMAPP
   :  forall s x12 t2      
   ,  STEP s (XAPP (XLAM x12) t2)
           s (substTX 0 t2 x12)

 (* Allocate a new data object in the store *)
 | EsAlloc
   :  forall s dc tsParam xs svs
   ,  Forall2 svalueOf xs svs
   -> STEP s                      (XCon dc tsParam xs)
           (snoc (SObj dc svs) s) (XLoc (length s))

 (* Case branching reads data objects from the store *)
 | EsCaseAlt
   :  forall s dc svs vs alts x l
   ,  get l s        = Some (SObj dc svs)
   -> getAlt dc alts = Some (AAlt dc x)
   -> Forall2 svalueOf vs svs
   -> STEP s (XCase (XLoc l) alts)
           s (substXXs 0 vs x)

 (* If an update operator matches the data constructor in the heap then
    update the appropriate field. *)
 | EsUpdate 
   :  forall l s dc cn i tsParam svs vField svField
   ,  get l s =  Some (SObj dc svs)
   -> dc      =  cn
   -> svalueOf vField svField
   -> STEP s
           (XUpdate cn i tsParam (XLoc l) vField)
           (replace l (SObj dc (replace i svField svs)) s)
           xUnit

 (* If an update operator does not match the data constructor in the heap
    then just return unit. *)
 | EsUpdateSkip
   :  forall l s dc cn i tsParam svs vField
   ,  get l s =  Some (SObj dc svs)
   -> ~(dc    = cn)
   -> STEP s (XUpdate cn i tsParam (XLoc l) vField)
           s xUnit.

Hint Constructors STEP.


(* Multi-step evaluation
   A sequence of small step transitions.
   As opposed to STEPSL, this version has an append constructor
   ESAppend that makes it easy to join two evaluations together.
   We use this when converting big-step evaluations to small-step. *)
Inductive STEPS : store -> exp -> store -> exp -> Prop :=

 (* After no steps, we get the same exp.
    We need this constructor to match the EVDone constructor
    in the big-step evaluation, so we can convert between big-step
    and multi-step evaluations. *)
 | EsNone
   :  forall s1 x1
   ,  STEPS s1 x1 s1 x1

 (* Take a single step. *)
 | EsStep
   :  forall s1 x1 s2 x2
   ,  STEP  s1 x1 s2 x2
   -> STEPS s1 x1 s2 x2

 (* Combine two evaluations into a third. *)
 | EsAppend
   :  forall s1 x1 s2 x2 s3 x3
   ,  STEPS s1 x1 s2 x2 -> STEPS s2 x2 s3 x3
   -> STEPS s1 x1 s3 x3.

Hint Constructors STEPS.


(* Stepping a wnf doesn't change it. *)
Lemma step_wnfX
 :  forall x v s1 s2
 ,  wnfX x -> STEP s1 x s2 v -> v = x.
Proof.
 intros x v s1 s2 HW HS.
 induction HS; nope.
  destruct H; auto; nope.
Qed.


(* If we have a list context we can step some expression 
   applied to a data constructor *)
Lemma step_context_XCon_exists
 :  forall  C x dc ts s1 s2
 ,  exps_ctx wnfX C 
 -> (exists x', STEP s1 x s2 x')
 -> (exists x', STEP s1 (XCon dc ts (C x)) s2 (XCon dc ts (C x'))).
Proof.
 intros C x dc ts s1 s2 HC HS.
 shift x'.
 eapply (EsContext (fun xx => XCon dc ts (C xx))); auto.
Qed.


(* Multi-step evaluating a wnf doesn't change it. *)
Lemma steps_wnfX 
 :  forall x v s1 s2
 ,  wnfX x -> STEPS s1 x s2 v -> v = x.
Proof.
 intros x v s1 s2 HW HS.
 induction HS; auto.
  Case "EsStep".
   eapply step_wnfX; eauto.
  
  Case "EsAppend".
   have (x2 = x1).
   subst. auto.
Qed.


(* Multi-step evaluation in a context. *)
Lemma steps_context
 :  forall C s1 x1 s2 x1'
 ,  exp_ctx C
 -> STEPS s1 x1     s2 x1'
 -> STEPS s1 (C x1) s2 (C x1').
Proof.
 intros C s1 x1 s2 x1' HC HS.
 induction HS; eauto.
Qed.


(* Multi-step evaluation of a data constructor argument. *)
Lemma steps_context_XCon
 :  forall C s1 x s2 v dc ts
 ,  exps_ctx wnfX C
 -> STEPS s1 x s2 v
 -> STEPS s1 (XCon dc ts (C x)) s2 (XCon dc ts (C v)).
Proof.
 intros C s1 x s2 v dc ts HC HS.
 induction HS; auto.

 Case "XCon".
  lets D: EsContext XcCon; eauto. 
  eauto.
Qed.

(* TODO: First premise doesn't work because each xs uses a different store
Lemma steps_in_XCon
 :  forall xs ts vs dc
 ,  Forall2 STEPS xs vs
 -> Forall wnfX vs
 -> STEPS (XCon dc ts xs) (XCon dc ts vs).
Proof.
 intros xs ts vs dc HS HW.
 lets HC: make_chain HS HW.
  eapply steps_wnfX.

 clear HS. clear HW.
 induction HC; auto.
  eapply (EsAppend (XCon dc ts (C x)) (XCon dc ts (C v))); auto.
  eapply steps_context_XCon; auto.
Qed.
*)

(********************************************************************)
(* Left linearised multi-step evaluation
   As opposed to STEPS, this version provides a single step at a time
   and does not have an append constructor. This is convenient
   when converting a small-step evaluations to big-step, via the
   eval_expansion lemma. *)
Inductive STEPSL : store -> exp -> store -> exp -> Prop :=
 | EslNone 
   : forall s1 x1
   , STEPSL s1 x1 s1 x1

 | EslCons
   :  forall s1 x1 s2 x2 s3 x3
   ,  STEP   s1 x1 s2 x2 -> STEPSL s2 x2 s3 x3 
   -> STEPSL s1 x1 s3 x3.

Hint Constructors STEPSL.


(* Transitivity of left linearised multi-step evaluation.
   We use this when "flattening" a big step evaluation to the
   small step one. *)
Lemma stepsl_trans
 :  forall s1 x1 s2 x2 s3 x3
 ,  STEPSL s1 x1 s2 x2 -> STEPSL s2 x2 s3 x3
 -> STEPSL s1 x1 s3 x3.
Proof.
 intros s1 x1 s2 x2 s3 x3 H1 H2.
 induction H1; eauto.
Qed.


(* Linearise a regular multi-step evaluation.
   This flattens out all the append constructors, leaving us with
   a list of individual transitions. *)
Lemma stepsl_of_steps
 :  forall s1 x1 s2 x2
 ,  STEPS  s1 x1 s2 x2
 -> STEPSL s1 x1 s2 x2.
Proof. 
 intros s1 x1 s2 x2 HS.
 induction HS; 
  eauto using stepsl_trans.
Qed.



Require Export DDC.Language.SimpleRef.SubstExpExp.
Require Export DDC.Language.SimpleRef.Exp.


(*******************************************************************)
(* Evaluation contexts for expressions.
   An evaluation context is an expression with a hole in any place
   that can take a step via our evaluatio rules. We represent
   the hole by the function that fills it. 
 *)
Inductive exp_ctx : (exp -> exp) -> Prop :=
 | XcTop
   :  exp_ctx (fun x => x)

 | XcApp1
   :  forall x2
   ,  exp_ctx (fun xx => XApp xx x2)

 | XcApp2 
   :  forall v1
   ,  value v1 
   -> exp_ctx (fun xx => XApp v1 xx)

 | XcNewRef
   :  exp_ctx (fun xx => XNewRef xx)

 | XcReadRef
   :  exp_ctx (fun xx => XReadRef xx)

 | XcWriteRef1
   :  forall x2
   ,  exp_ctx (fun xx => XWriteRef xx x2)

 | XcWriteRef2
   :  forall v1
   ,  value v1
   -> exp_ctx (fun xx => XWriteRef v1 xx).

Hint Constructors exp_ctx.



(* Small Step evaluation *)
Inductive STEP  : heap -> exp -> heap -> exp -> Prop :=

 (* Evaluation in a context. *)
 | EsContext 
   :  forall C h x h' x'
   ,  exp_ctx C
   -> STEP h x     h' x'
   -> STEP h (C x) h' (C x')

 (* Function application. *)
 | EsLamApp 
   :  forall h t11 x12 v2
   ,  value v2
   -> STEP h (XApp (XLam t11 x12) v2)
           h (substX 0 v2 x12)

 (* References *)
 | EsNewRef
   :  forall h v1
   ,  value v1
   -> STEP h           (XNewRef v1)
           (snoc v1 h) (XLoc (length h))

 | EsReadRef
   :  forall l h v
   ,  get l h = Some v
   -> STEP h  (XReadRef (XLoc l))
           h  v

 | EsWriteRef
   :  forall l h v2
   ,  STEP h  (XWriteRef (XLoc l) v2)
           (update l v2 h) xUnit.

Hint Constructors STEP.


(********************************************************************)
(* Multi-step evaluation
   A sequence of small step transitions.
   As opposed to STEPSL, this version has an append constructor
   EsAppend that makes it easy to join two evaluations together.
   We use this when converting big-step evaluations to small-step.
 *)
Inductive STEPS : heap -> exp -> heap -> exp -> Prop :=

 (* After no steps, we get the same exp.
    We need this constructor to match the EVDone constructor
    in the big-step evaluation, so we can convert between big-step
    and multi-step evaluations. *)
 | EsNone
   :  forall h x
   ,  STEPS h x  h x

 (* Take a single step. *)
 | EsStep
   :  forall h1 x1 h2 x2
   ,  STEP  h1 x1 h2 x2
   -> STEPS h1 x1 h2 x2

 (* Combine two evaluations into a third. *)
 | EsAppend
   :  forall h1 x1 h2 x2 h3 x3
   ,  STEPS h1 x1 h2 x2 -> STEPS h2 x2 h3 x3
   -> STEPS h1 x1 h3 x3.

Hint Constructors STEPS.


(* Multi-step evaluation in a context.
   If an expression can be evaluated at top level, then it can 
   be evaluated to the same result in any evaluation context. *)
Lemma steps_context
 :  forall C h x h' x'
 ,  exp_ctx C
 -> STEPS h x     h' x'
 -> STEPS h (C x) h' (C x').
Proof.
 intros.
 induction H0; eauto.
Qed.


(********************************************************************)
(* Left linearised multi-step evaluation
   As opposed to STEPS, this version provides a single step at a time
   and does not have an append constructor. This is convenient
   when converting a small-step evaluations to big-step, via the
   eval_expansion lemma.
 *)
Inductive STEPSL : heap -> exp -> heap -> exp -> Prop :=

 | EslNone 
   : forall h x
   , STEPSL h x h x

 | EslCons
   :  forall h1 x1 h2 x2 h3 x3
   ,  STEP   h1 x1 h2 x2 -> STEPSL h2 x2 h3 x3 
   -> STEPSL h1 x1 h3 x3.

Hint Constructors STEPSL.


(* Transitivity of left linearised multi-step evaluation.
   We use this when "flattening" a big step evaluation to the
   small step one.
 *)
Lemma stepsl_trans
 :  forall h1 x1 h2 x2 h3 x3
 ,  STEPSL h1 x1 h2 x2 -> STEPSL h2 x2 h3 x3
 -> STEPSL h1 x1 h3 x3.
Proof.
 intros.
 induction H; eauto.
Qed.


(* Linearise a regular multi-step evaluation.
   This flattens out all the append constructors, leaving us with
   a list of individual transitions. 
 *)
Lemma stepsl_of_steps
 :  forall h1 x1 h2 x2
 ,  STEPS  h1 x1 h2 x2
 -> STEPSL h1 x1 h2 x2.
Proof. 
 intros.
 induction H; eauto using stepsl_trans.
Qed.


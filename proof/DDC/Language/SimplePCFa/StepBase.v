
Require Export DDC.Language.SimplePCFa.Exp.
Require Export DDC.Language.SimplePCFa.ExpLift.
Require Export DDC.Language.SimplePCFa.ExpSubst.


(******************************************************************************)
(* Primitive reduction step. *)
Inductive STEPP : exp -> exp -> Prop := 
 (* Application *)
 | SpAppLam
   : forall t11 x12 v2
   , STEPP (XApp (VLam t11 x12) v2)
           (substVX 0 v2 x12)

 | SpAppFix 
   : forall t1 v2 v3
   , STEPP (XApp (VFix t1 v2) v3)
           (XApp (substVV 0 (VFix t1 v2) v2) v3)

 (* Naturals *)
 | SpSucc 
   : forall n
   , STEPP (XOp1 OSucc (VConst (CNat n)))
           (XVal (VConst (CNat (S n))))

 | SpPredZero
   : STEPP (XOp1 OPred (VConst (CNat O)))
           (XVal (VConst (CNat O)))

 | SpPredSucc
   : forall n
   , STEPP (XOp1 OPred (VConst (CNat (S n))))
           (XVal (VConst (CNat n)))

 (* Booleans *)
 | SpIsZeroTrue
   : STEPP (XOp1 OIsZero (VConst (CNat O)))
           (XVal (VConst (CBool true)))

 | SpIsZeroFalse
   : forall n
   , STEPP (XOp1 OIsZero (VConst (CNat (S n))))
           (XVal (VConst (CBool false)))

 (* Branching *)
 | SpIfThen
   : forall x1 x2
   , STEPP (XIf (VConst (CBool true)) x1 x2) x1

 | SpIfElse
   : forall x1 x2
   , STEPP (XIf (VConst (CBool false)) x1 x2) x2.
Hint Constructors STEPP.


(* Single step reduction. 
   This judgement contains the rule for 'let', which is the only form
   that holds a context while it reduces an expression. *)
Inductive STEP : exp -> exp -> Prop :=
 | SPrim 
   :  forall x1 x2
   ,  STEPP x1 x2
   -> STEP  x1 x2

 | SLetStep
   :  forall t1 x1 x1' x2
   ,  STEP x1 x1'
   -> STEP (XLet t1 x1  x2)
           (XLet t1 x1' x2)

 | SLetSub
   : forall t1 v1 x2 
   , STEP (XLet t1 (XVal v1) x2)
          (substVX 0 v1 x2).
Hint Constructors STEP.


(********************************************************************)
(** Multi-step evaluation. *)
Inductive STEPS : exp -> exp -> Prop :=
 | SsNone
   :  forall x1
   ,  STEPS x1 x1

 (* Take a single step. *)
 | SsStep
   :  forall x1 x2
   ,  STEP  x1 x2
   -> STEPS x1 x2

 (* Combine two evaluations into a third. *)
 | SsAppend
   :  forall x1 x2 x3
   ,  STEPS x1 x2 -> STEPS x2 x3
   -> STEPS x1 x3.

Hint Constructors STEPS.


(* TODO *)
Lemma steps_context_let1
 :  forall t1 x1 x1' x2
 ,  STEPS x1 x1' 
 -> STEPS (XLet t1 x1 x2) (XLet t1 x1' x2).
Proof.
 admit.
Qed.




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


(* TODO: shift this to steps *)
Lemma steps_context_let1
 :  forall t1 x1 x1' x2
 ,  STEPS x1 x1' 
 -> STEPS (XLet t1 x1 x2) (XLet t1 x1' x2).
Proof.
 admit.
Qed.


(******************************************************************************)
(* Frame Stacks *)
(* Holds the continuation while a 'let' expression reduces the bound term. *)
Inductive frame : Type :=
 | FNil     : frame
 | FLet     : frame -> ty -> exp -> frame.
Hint Constructors frame.


(* Termination of an expression reduction in some context. *)
Inductive TERMF : frame -> exp -> Prop :=
 (* A value in an empty context has terminated. *)
 | RfVal 
   : forall v
   , TERMF FNil (XVal v)

 | RfLetPush
   :  forall f t1 x1 x2
   ,  TERMF (FLet f t1 x2) x1
   -> TERMF f (XLet t1 x1 x2)

 | RfLetPop
   :  forall f t1 v1 x2
   ,  TERMF f (substVX 0 v1 x2)
   -> TERMF (FLet f t1 x2) (XVal v1)

 | RfStep
   :  forall f x1 x1'
   ,  STEPP x1 x1' -> TERMF f x1'
   -> TERMF f x1.
Hint Constructors TERMF.


(* Termination of an expression reduction in an empty context. *)
Inductive TERM : exp -> Prop :=
 | RTerm
   : forall x
   , TERMF FNil x -> TERM x.
Hint Constructors TERM.


(* Wrap a frame context around an expression.
   This converts the frame to its expression form. *)
Fixpoint wrap (f: frame ) (x1: exp) : exp :=
 match f with
 | FNil          => x1
 | FLet f' t1 x2 => wrap f' (XLet t1 x1 x2)
 end.


(* Reducing a term with an explicit frame is equivalent
   to wrapping the term with its frame and reducing that. *)
Lemma frame_wrap
 : forall f x
 , TERMF f x <-> TERM (wrap f x).
Proof.
 intros.
 split.

  intros. gen x.
  induction f; eauto.

  intros. gen x.
  induction f; intros.
   simpl in H. inverts H. auto.

   lets D: IHf H.
   clear IHf. clear H.
   destruct x;
    inverts D; eauto; nope.
Qed.  

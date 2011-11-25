
Require Import DDC.Language.SimplePCFa.StepTerm.
Require Import DDC.Language.SimplePCFa.StepBase.
Require Import DDC.Language.SimplePCFa.Exp.
Require Import DDC.Language.SimplePCFa.ExpSubst.


(********************************************************************)
(* Big Step Evaluation.
   This is also called 'Natural Semantics'.
   It provides a relation between the expression to be reduced 
   and its final value. 
 *)
Inductive EVAL : exp -> exp -> Prop :=

 (* Values are already evaluated ******)
 | EvDone
   : forall v2
   , EVAL (XVal v2) (XVal v2)

 (* Let Evaluation ********************)
 | EvLet
   :  forall t1 x1 v1 x2 x3
   ,  EVAL x1                (XVal v1)
   -> EVAL (substVX 0 v1 x2) x3
   -> EVAL (XLet t1 x1 x2)   x3

 (* Function Applications *************)
 | EvAppLam
   :  forall t11 x12 v2 x3
   ,  EVAL (substVX 0 v2 x12) x3
   -> EVAL (XApp (VLam t11 x12) v2) x3

 | EvFunApp
   :  forall t11 v12 v2 x3
   ,  EVAL (XApp (substVV 0 (VFix t11 v12) v12) v2) x3
   -> EVAL (XApp (VFix t11 v12) v2) x3

 (* Naturals **************************)
 | EvSucc
   :  forall n
   ,  EVAL (XOp1 OSucc (VConst (CNat n)))
           (XVal (VConst (CNat (S n))))

 | EvPredZero
   :  EVAL (XOp1 OPred (VConst (CNat O))) 
           (XVal (VConst (CNat O)))

 | EvPredSucc
   :  forall n
   ,  EVAL (XOp1 OPred (VConst (CNat (S n))))
           (XVal (VConst (CNat n)))

 (* Booleans **************************)
 | EvIsZeroTrue
   :  EVAL (XOp1 OIsZero (VConst (CNat O)))
           (XVal (VConst (CBool true)))

 | EvIsZeroFalse
   :  forall n
   ,  EVAL (XOp1 OIsZero (VConst (CNat (S n))))
           (XVal (VConst (CBool false)))

 (* Branching *************************)
 | EvIfThen
   :  forall x2 x2' x3
   ,  EVAL x2 x2'
   -> EVAL (XIf (VConst (CBool true))  x2 x3) x2'

 | EvIfElse
   :  forall x2 x3 x3'
   ,  EVAL x3 x3'
   -> EVAL (XIf (VConst (CBool false)) x2 x3) x3'.

Hint Constructors EVAL.


Lemma eval_value
 :  forall x1 x2
 ,  EVAL x1 x2
 -> (exists v2, x2 = XVal v2).
Proof.
 intros.
 induction H; eauto.
Qed.
Hint Resolve eval_value.


(*****************************************************************************)
(* Eval to steps *)
Lemma eval_steps
 :  forall x1 x2
 ,  EVAL  x1 x2 
 -> STEPS x1 x2.
Proof.
 admit.
Qed.
Hint Resolve eval_steps.


Lemma steps_eval
 :  forall x1 x2
 ,  STEPS x1 x2
 -> EVAL  x1 x2.
Proof.
 admit.
Qed.
Hint Resolve steps_eval.


(******************************************************************************)
(* Termination *)
Lemma term_steps_exists
 :  forall x
 ,  TERM x 
 -> (exists x', STEPS x x').
Proof.
 intros.
 inverts H.
 induction H0; eauto.
Qed.
Hint Resolve term_steps_exists.


Lemma term_eval_exists
 :  forall x
 ,  TERM x 
 -> (exists x', EVAL x x').
Proof.
 intros.
 have (exists x', STEPS x x').
 shift x'.
 eauto.
Qed.


Lemma term_eval_wrapped
 :  forall x1 f
 ,  TERMS f x1
 -> (exists x2, EVAL (wrap f x1) x2).
Proof. eauto. Qed.



Lemma term_steps_rewind
 : forall f x1 x2
 , TERMS f x2 -> STEPS x1 x2 -> TERMS f x1.
Proof.
 intros.
 induction H0; eauto.
 admit.  (* TODO *)
Qed.


(******************************************************************************)

(* If we have a big-step evaluation for some expression, 
   then that expression terminates. *)
Lemma eval_term
 : forall x1 x2
 , EVAL x1 x2 -> TERM x1.
Proof.
 intros. 
 induction H; eauto.

 Case "XLet".
  eapply RTerm.
  inverts IHEVAL1.
  inverts IHEVAL2.
  eapply term_steps_rewind.
  eauto.
  have (STEPS x1 (XVal v1)) 
   by (eapply eval_steps; auto).
  eapply SsAppend.
  eapply steps_context_let1. eauto.
  eapply SsStep.
  eauto.

 Case "XApp/VLam".
  eapply RTerm.
  eapply RfStep.
  eapply SpAppLam.
  inverts IHEVAL. eauto.

 Case "XApp/VFix".
  eapply RTerm.
  eapply RfStep.
  eapply SpAppFix.
  inverts IHEVAL. eauto.

 Case "XIfThen".
  inverts IHEVAL. eauto.
  inverts IHEVAL. eauto.
Qed.


Lemma eval_term_wrapped
 :  forall x1 x2 f
 ,  EVAL (wrap f x1) x2
 -> TERMS f x1.
Proof. 
 intros.
 eapply eval_term in H.
 gen x1. 
 induction f; intros.
  simpl in *. inverts H. auto.
  simpl in *.
  destruct a. 
  lets D: IHf H. inverts D. auto.
   inverts H0.
Qed.
Hint Resolve eval_term_wrapped.


Lemma eval_eval_wrapped
 : forall x1 x2 f
 , EVAL (wrap f x1) x2 -> (exists x3, EVAL x1 x3).
Proof. burn. Qed.
Hint Resolve eval_eval_wrapped.


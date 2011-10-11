
Require Export DDC.Language.Simple.Exp.

(* Typing judgement assigns a type to an expression. *)
Inductive TYPE (te: tyenv) : exp -> ty -> Prop :=
 | TYVar 
   :  forall i t
   ,  get i te = Some t
   -> TYPE te (XVar i) t

 | TYLam
   :  forall x t1 t2
   ,  TYPE (te :> t1) x t2
   -> TYPE te (XLam t1 x) (TFun t1 t2)

 | TYApp
   :  forall x1 x2 t1 t2
   ,  TYPE te x1 (TFun t1 t2)
   -> TYPE te x2 t1
   -> TYPE te (XApp x1 x2) t2.

Hint Constructors TYPE.


(* Invert all hypothesis that are compound typing statements. *)
Ltac inverts_type :=
 repeat 
  (match goal with 
   | [ H: TYPE _ (XVar _)   _ |- _ ] => inverts H
   | [ H: TYPE _ (XLam _ _) _ |- _ ] => inverts H
   | [ H: TYPE _ (XApp _ _) _ |- _ ] => inverts H
   end).


(* Induction over structure of expression, 
   inverting compound typing judgements along the way.
   This gets common cases in proofs about TYPE judgements. *)
Tactic Notation "induction_type" ident(X) :=
 induction X; intros; inverts_type; simpl; eauto.


(*******************************************************************)
(* Forms of values. 
   If we know the type of a value then we know the
   form of that value. *)

Lemma value_lam 
 :  forall xx te t1 t2
 ,  value xx 
 -> TYPE te xx (TFun t1 t2)
 -> (exists t x, xx = XLam t x).
Proof. 
 destruct xx; eauto; nope.
Qed.
Hint Resolve value_lam.


(********************************************************************)
(* A well typed expression is well formed. *)
Theorem type_wfX
 :  forall te x t
 ,  TYPE te x t
 -> wfX  te x.
Proof.
 intros. gen te t.
 induction_type x.
Qed.
Hint Resolve type_wfX.


(* Weakening the type environment of a typing judgement.
   We can insert a new type into the type environment, provided we
   lift existing references to types higher in the stack across
   the new one. *)
Lemma type_tyenv_insert
 :  forall te ix x t1 t2
 ,  TYPE te x t1
 -> TYPE (insert ix t2 te) (liftX ix x) t1.
Proof.
 intros. gen ix te t1.
 induction_type x.

 Case "XVar".
  lift_cases; intros; auto.

 Case "XLam".
  apply TYLam.
  rewrite insert_rewind. auto. 
Qed.
Hint Resolve type_tyenv_insert.


Lemma type_tyenv_weaken
 :  forall te x t1 t2
 ,  TYPE  te         x          t1
 -> TYPE (te :> t2) (liftX 0 x) t1.
Proof.
 rip. rw (te :> t2 = insert 0 t2 te). auto.
Qed.
Hint Resolve type_tyenv_weaken.


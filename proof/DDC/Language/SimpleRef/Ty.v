
Require Export DDC.Language.SimpleRef.Exp.

(* Typing judgement assigns a type to an expression. *)
Inductive TYPE : tyenv -> stenv -> exp -> ty -> Prop :=
 | TyVar 
   :  forall te se i t
   ,  get i te = Some t
   -> TYPE te se (XVar i) t

 | TyLoc
   :  forall te se i t
   ,  get i se = Some t
   -> TYPE te se (XLoc i) (TRef t)

 | TyLam
   :  forall te se x t1 t2
   ,  TYPE (te :> t1) se x t2
   -> TYPE te se (XLam t1 x) (TFun t1 t2)

 | TyApp
   :  forall te se x1 x2 t1 t2
   ,  TYPE te se x1 (TFun t1 t2)
   -> TYPE te se x2 t1
   -> TYPE te se (XApp x1 x2) t2

 (* As these primitives are polymorphic, 
    but we're using a monomorphic ambient calculus
    we must give them their own rules. *)
 | TyNewRef
   :  forall te se xData tData
   ,  TYPE te se xData tData
   -> TYPE te se (XNewRef xData)  (TRef tData)

 | TyReadRef
   :  forall te se xRef tData
   ,  TYPE te se xRef (TRef tData)
   -> TYPE te se (XReadRef xRef)  tData

 | TyWriteRef
   :  forall te se xRef xData tData 
   ,  TYPE te se xRef  (TRef tData)
   -> TYPE te se xData tData
   -> TYPE te se (XWriteRef xRef xData) tUnit.

Hint Constructors TYPE.


(* Invert all hypothesis that are compound typing statements. *)
Ltac inverts_type :=
 repeat 
  (match goal with 
   | [ H: TYPE _ _ (XCon _)   _      |- _ ] => inverts H
   | [ H: TYPE _ _ (XVar _)   _      |- _ ] => inverts H
   | [ H: TYPE _ _ (XLam _ _) _      |- _ ] => inverts H
   | [ H: TYPE _ _ (XApp _ _) _      |- _ ] => inverts H
   | [ H: TYPE _ _ (XNewRef _) _     |- _ ] => inverts H
   | [ H: TYPE _ _ (XWriteRef _ _) _ |- _ ] => inverts H
   | [ H: TYPE _ _ (XReadRef _ ) _   |- _ ] => inverts H
   | [ H: TYPE _ _ (XLoc _ ) _       |- _ ] => inverts H
   end).


(* Induction over structure of expression, 
   inverting compound typing judgements along the way.
   This gets common cases in proofs about TYPE judgements. *)
Tactic Notation "induction_type" ident(X) :=
 induction X; intros; inverts_type; simpl; eauto.


(********************************************************************)
(* Well-typing of heap wrt the store typing. *)
Definition TYPEH (se : tyenv) (h: heap)
   := Forall2 (TYPE nil se) h se.


(********************************************************************)
(* Forms of values. 
   If we know the type of a value,
   then we know the form of that value. *)

Lemma value_lam 
 :  forall x te se t1 t2
 ,  value x 
 -> TYPE te se x (TFun t1 t2)
 -> (exists t x', x = XLam t x').
Proof.
 intros. destruct x; eauto; nope.
Qed.
Hint Resolve value_lam.


Lemma value_ref
 :  forall x te se t
 ,  value x
 -> TYPE te se x (TRef t)
 -> (exists l, x = XLoc l).
Proof.
 intros. destruct x; eauto; nope.
Qed.
Hint Resolve value_ref.


(********************************************************************)
(* A well typed expression is well formed. *)
Theorem type_wfX
 :  forall te se x t
 ,  TYPE te se x t
 -> wfX  te x.
Proof.
 intros. induction H; simpl; eauto.
Qed.
Hint Resolve type_wfX.


(********************************************************************)
(* Weakening the type environment of a typing judgement.
   We can insert a new type into the type environment, provided we
   lift existing references to types higher in the stack across
   the new one. *)
Lemma type_tyenv_insert
 :  forall te se ix x t1 t2
 ,  TYPE te se x t1
 -> TYPE (insert ix t2 te) se (liftX ix x) t1.
Proof.
 intros. gen ix te se t1.
 induction_type x.

 Case "XVar".
  lift_cases; intros; auto.

 Case "XLam".
  apply TyLam.
  rewrite insert_rewind. eauto. 
Qed.


Lemma type_tyenv_weaken
 :  forall te se x t1 t2
 ,  TYPE  te se         x          t1
 -> TYPE (te :> t2) se (liftX 0 x) t1.
Proof.
 intros.
 assert (te :> t2 = insert 0 t2 te).
  simpl. destruct te; auto.
  rewrite H0. apply type_tyenv_insert. auto.
Qed.


(********************************************************************)
(* Weakening of store typing. *)
Lemma type_stenv_push
 : forall te se1 t2     x t1
 ,  TYPE te  se1        x t1
 -> TYPE te (t2 <: se1) x t1.
Proof.
 intros. gen te t1.
 induction_type x.
Qed.
Hint Resolve type_stenv_push.


Lemma type_stenv_weaken
 : forall te se1 se2 x t1
 ,  TYPE te  se1         x t1
 -> TYPE te (se2 >< se1) x t1.
Proof.
 intros. gen se1.
 induction se2; intros.
 rewrite app_nil_right. auto.
 assert ((se2 :> a) >< se1 = se2 >< (a <: se1)). 
  auto. rewrite H0.
 eauto.
Qed.
Hint Resolve type_stenv_weaken.


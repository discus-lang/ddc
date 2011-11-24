
Require Import DDC.Language.SimplePCFa.Step.
Require Import DDC.Language.SimplePCFa.ExpSubst.
Require Import DDC.Language.SimplePCFa.Exp.


(* Frame stacks *)
(* Holds the continuation while a 'let' expression reduces the bound term. *)
Inductive frame : Set :=
 | F : forall (t : ty) (x : exp), frame.
Hint Constructors frame.

Definition stack := list frame.
Hint Unfold stack.


(* Termination of an expression reduction in some context. *)
Inductive TERMS : stack -> exp -> Prop :=
 (* A value in an empty context has terminated. *)
 | RfVal 
   :  forall v
   ,  TERMS nil (XVal v)

 | RfLetPush
   :  forall f t1 x1 x2
   ,  TERMS (f :> F t1 x2) x1
   -> TERMS f (XLet t1 x1 x2)

 | RfLetPop
   :  forall f t1 v1 x2
   ,  TERMS f (substVX 0 v1 x2)
   -> TERMS (f :> F t1 x2) (XVal v1)

 | RfStep
   :  forall f x1 x1'
   ,  STEPP x1 x1' -> TERMS f x1'
   -> TERMS f x1.
Hint Constructors TERMS.


(* Termination of an expression reduction in an empty context. *)
Inductive TERM : exp -> Prop :=
 | RTerm
   : forall x
   , TERMS nil x -> TERM x.
Hint Constructors TERM.


(* Wrap a frame context around an expression.
   This converts the frame to its expression form. *)
Fixpoint wrap (fs: stack) (x1: exp) : exp :=
 match fs with
 | nil           => x1
 | f' :> F t1 x2 => wrap f' (XLet t1 x1 x2)
 end.


(* Reducing a term with an explicit frame is equivalent
   to wrapping the term with its frame and reducing that. *)
Lemma terms_wrap
 : forall f x
 , TERMS f x <-> TERM (wrap f x).
Proof.
 intros.
 split.

  intros. gen x.
  induction f; eauto.
   intros. induction x; simpl; destruct a; eauto.

  intros. gen x.
  induction f; intros.
   simpl in H. inverts H. auto.
   simpl in H. destruct a.
    lets D: IHf H.
    destruct x; inverts D; nope.
Qed.  



Require Export DDC.Language.Simple.Exp.

(* Typing judgement assigns a type to an expression. *)
Inductive TYPE : tyenv -> exp -> ty -> Prop :=
 | TYVar 
   :  forall te i t
   ,  get i te = Some t
   -> TYPE te (XVar i) t

 | TYLam
   :  forall te x t1 t2
   ,  TYPE (te :> t1) x t2
   -> TYPE te (XLam t1 x) (TFun t1 t2)

 | TYApp
   :  forall te x1 x2 t1 t2
   ,  TYPE te x1 (TFun t1 t2)
   -> TYPE te x2 t1
   -> TYPE te (XApp x1 x2) t2.

Hint Constructors TYPE.


(* Lemmas ***********************************************************)
(* A well typed expression is well formed. *)
Theorem type_wfX
 :  forall te x t
 ,  TYPE te x t
 -> wfX  te x.
Proof.
 intros. gen te t.
 induction x; intros; inverts H; simpl; eauto.
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
 induction x; intros; simpl; inverts H; eauto.

 Case "XVar".
  lift_cases; intros; auto.

 Case "XLam".
  apply TYLam.
  rewrite insert_rewind. 
   apply IHx. auto.
Qed.


Lemma type_tyenv_weaken
 :  forall te x t1 t2
 ,  TYPE  te         x          t1
 -> TYPE (te :> t2) (liftX 0 x) t1.
Proof.
 intros.
 assert (te :> t2 = insert 0 t2 te).
  simpl. destruct te; auto.
  rewrite H0. apply type_tyenv_insert. auto.
Qed.



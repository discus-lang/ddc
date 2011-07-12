
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


(********************************************************************)
Definition TYPEH (se : tyenv) (h: heap)
   := Forall2 (TYPE nil nil) h se.


(********************************************************************)
(* A well typed expression is well formed. *)
Theorem type_wfX
 :  forall te se x t
 ,  TYPE te se x t
 -> wfX  te x.
Proof.
 intros. 
 induction H; simpl; eauto.
Qed.
Hint Resolve type_wfX.


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
 induction x; intros; simpl; inverts H; eauto.

 Case "XVar".
  lift_cases; intros; auto.

 Case "XLam".
  apply TyLam.
  rewrite insert_rewind. 
   apply IHx. auto.
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



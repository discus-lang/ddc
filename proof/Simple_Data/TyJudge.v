
Require Export Exp.


(** Type Judgements *************************************************)
Inductive TYPE : defs -> tyenv -> exp -> ty -> Prop :=
 (* Variables *)
 | TYVar 
   :  forall ds te i t
   ,  get te i = Some t
   -> TYPE ds te (XVar i) t

 (* Lambda Abstraction *)
 | TYLam
   :  forall ds te x t1 t2
   ,  TYPE ds (te :> t1) x            t2
   -> TYPE ds te         (XLam t1 x) (TFun t1 t2)

 (* Applications *)
 | TYApp
   :  forall ds te x1 x2 t1 t2
   ,  TYPE ds te x1           (TFun t1 t2)
   -> TYPE ds te x2           t1
   -> TYPE ds te (XApp x1 x2) t2

 (* Data Constructors *)
 | TYCon 
   :  forall ds te xs dc tsArgs tResult
   ,  getDataDef  dc ds = Some (DefData dc tsArgs tResult)
   -> (Forall2 (TYPE ds te) xs tsArgs)
   -> TYPE ds te (XCon dc xs) tResult

 | TYCase
   :  forall ds te xObj alts tPat tResult
   ,  TYPE ds te xObj tPat
   -> (forall a, In a alts -> TYPEA ds te a tPat tResult)
   -> TYPE ds te (XCase xObj alts) tResult

with TYPEA : defs -> tyenv -> alt -> ty -> ty -> Prop :=
 (* Case Alternatives *)
 | TYAlt 
   :  forall ds te x1 t1 dc
   ,  TYPE  ds te x1 t1
   -> TYPEA ds te (AAlt dc x1) (TCon (TyConData "dude")) t1.

Hint Constructors TYPE.
Hint Constructors TYPEA.


(* Well Formedness **************************************************)
(* A well typed expression is well formed *)
Theorem type_wfX
 :  forall ds te x t
 ,  TYPE ds te x t
 -> wfX te x.
Proof.
 intros ds te x t. gen ds te t.

 eapply 
  (exp_alt_mutind 
    (fun x => forall ds te t,     TYPE  ds te x t     -> wfX te x) 
    (fun a => forall ds te t1 t2, TYPEA ds te a t1 t2 -> wfA te a)).

 ; intros; try (inverts H).

 Case "XVar".
  eauto.

 Case "XLam".
  inverts H0. eauto.

 Case "XApp".
  inverts H1. eauto.

 Case "XCon".
  admit.

 Case "XCase".
  inverts H0.
  eapply WfX_XCase.
   eauto.
   rewrite Forall_forall.
    intros.
    lets D: H7 H0.
     inverts D.
     eapply WfA_AAlt.
     eapply H7 in H0. inverts H0.





Qed.
Hint Resolve type_wfX.


(* Weakening Type Env in Type Judgement *****************************
   We can insert a new type into the type environment, provided we
   lift existing references to types higher in the stack across
   the new one.
 *)
Lemma type_tyenv_insert
 :  forall e ix x t1 t2
 ,  TYPE e x t1
 -> TYPE (insert ix t2 e) (liftX ix x) t1.
Proof.
 intros. gen ix e t1.
 induction x; intros; simpl; inverts H; eauto.

 Case "XVar".
  lift_cases; intros; auto.

 Case "XLam".
  apply TYLam.
  rewrite insert_rewind. 
   apply IHx. auto.
Qed.


Lemma type_tyenv_weaken
 :  forall e x t1 t2
 ,  TYPE  e         x            t1
 -> TYPE (e :> t2) (liftX 0 x) t1.
Proof.
 intros.
 assert (e :> t2 = insert 0 t2 e).
  simpl. destruct e; auto.
  rewrite H0. apply type_tyenv_insert. auto.
Qed.



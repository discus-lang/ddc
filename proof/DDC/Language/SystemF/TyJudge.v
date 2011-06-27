
Require Import DDC.Language.SystemF.SubstTypeType.
Require Export DDC.Language.SystemF.KiJudge.
Require Export DDC.Language.SystemF.TyEnv.
Require Export DDC.Language.SystemF.Exp.


(* Type judgement assigns a type to an expression. *)
Inductive TYPE : kienv -> tyenv -> exp -> ty -> Prop :=
 | TYVar 
   :  forall i ke te t
   ,  get i te = Some t
   -> KIND ke t  KStar
   -> TYPE ke te (XVar i) t

 | TYLam 
   :  forall ke te x12 t11 t12
   ,  KIND ke t11 KStar
   -> TYPE ke (te :> t11)  x12            t12
   -> TYPE ke  te         (XLam t11 x12) (TFun t11 t12)

 | TYApp 
   :  forall ke te x1 x2 t11 t12
   ,  TYPE ke te x1 (TFun t11 t12) 
   -> TYPE ke te x2 t11
   -> TYPE ke te (XApp x1 x2) t12

 | TYLAM
   :  forall ke te x1 t1
   ,  TYPE (ke :> KStar) (liftTE 0 te) x1        t1
   -> TYPE ke            te           (XLAM x1) (TForall t1)

 | TYAPP
   :  forall ke te x1 t1 t2
   ,  TYPE ke te x1 (TForall t1)
   -> KIND ke t2 KStar
   -> TYPE ke te (XAPP x1 t2) (substTT 0 t2 t1). 

Hint Constructors TYPE.


(* The type produced by a type judgement is well kinded. *)
Theorem type_kind
 :  forall ke te x t
 ,  TYPE ke te x t
 -> KIND ke t KStar.
Proof.
 intros. gen ke te t.
 induction x; intros; inverts H; eauto.
 
 Case "XAPP".
  apply IHx in H4. inverts H4.
  eapply subst_type_type; eauto.

 Case "XLam".
  eapply IHx1 in H4. inverts H4. auto.
Qed.


(* A well typed expression is well formed. *)
Theorem type_wfX
 :  forall ke te x t
 ,  TYPE ke te x t
 -> wfX  ke te x.
Proof.
 intros. gen ke te t.
 induction x; intros; simpl.

 Case "XVar".
  inverts H. eauto.

 Case "XLAM".
  inverts H.
  apply IHx in H3. eauto.

 Case "XAPP".
  inverts H. 
  lets D: IHx H4. split. 
   auto. eapply kind_wfT. eauto.

 Case "XLam".
  inverts H.
  apply IHx in H6.
  apply kind_wfT in H4. auto.

 Case "XApp".
  inverts H.
  apply IHx1 in H4. 
  apply IHx2 in H6.
   auto.
Qed.
Hint Resolve type_wfX.


(********************************************************************)
(* Weakening the kind environment of a type judgement.
   We can insert a new kind into the kind environment of a type
   judgement, provided we lift existing references to kinds higher
   than this in the stack over the new one.

   References to existing elements of the kind environment may
   appear in the type environment, expression, as well as the
   resulting type -- so we must lift all of them.
 *)
Lemma type_kienv_insert
 :  forall ix ke te x1 t1 k2
 ,  TYPE ke                 te             x1             t1
 -> TYPE (insert ix k2 ke) (liftTE ix te) (liftTX ix x1) (liftTT ix t1).
Proof. 
 intros. gen ix ke te t1 k2.
 induction x1; intros; inverts H; simpl; eauto.

 Case "XVar".
  apply TYVar. 
  apply get_map. auto.
  apply liftTT_insert. auto.

 Case "XLAM".
  eapply TYLAM. 
  rewrite insert_rewind. 
   rewrite (liftTE_liftTE 0 ix).
   apply IHx1. auto.

 Case "XAPP".
  rewrite (liftTT_substTT' 0 ix). simpl.
  eapply TYAPP.
  eapply (IHx1 ix) in H4. simpl in H4. eauto.
  apply liftTT_insert. auto.

 Case "XLam".
  apply TYLam.
   apply liftTT_insert. auto.
   assert ( liftTE ix te :> liftTT ix t
          = liftTE ix (te :> t)). auto. rewrite H. clear H.
   apply IHx1. auto.

 Case "XApp".
  eapply TYApp.
   eapply IHx1_1 in H4. simpl in H4. eauto.
   eapply IHx1_2 in H6. eauto.
Qed.


Lemma type_kienv_weaken
 :  forall ke te x1 t1 k2
 ,  TYPE ke         te            x1            t1
 -> TYPE (ke :> k2) (liftTE 0 te) (liftTX 0 x1) (liftTT 0 t1).
Proof.
 intros.
 assert (ke :> k2 = insert 0 k2 ke). 
  destruct ke; auto. rewrite H0.
  apply type_kienv_insert. auto.
Qed.


(********************************************************************)
(* Weakening the type environment of a type judgement. 
   We can insert a new type into the type environment of a type 
   judgement, provided we lift existing references to types higher
   than this in the stack over the new one.
 *)
Lemma type_tyenv_insert
 :  forall ke te ix x1 t1 t2
 ,  TYPE ke  te                x1            t1
 -> TYPE ke (insert ix t2 te) (liftXX ix x1) t1.
Proof. 
 intros. gen ix ke te t1 t2.
 induction x1; intros; simpl; inverts H; eauto.

 Case "XVar".
  lift_cases; intros; auto.

 Case "XLAM".
  apply TYLAM. simpl.
  assert ( liftTE 0 (insert ix t2 te)
         = insert ix (liftTT 0 t2) (liftTE 0 te)). 
   unfold liftTE. rewrite map_insert. auto.
  rewrite H.
  apply IHx1. auto.

 Case "XLam".
  eapply TYLam.
   auto.
   rewrite insert_rewind. apply IHx1. auto.
Qed.


Lemma type_tyenv_weaken
 :  forall ke te x1 t1 t2
 ,  TYPE ke  te         x1           t1
 -> TYPE ke (te :> t2) (liftXX 0 x1) t1.
Proof.
 intros.
 assert (te :> t2 = insert 0 t2 te).
  destruct te; auto. rewrite H0.
  apply type_tyenv_insert. auto.
Qed.




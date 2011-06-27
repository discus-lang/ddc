
Require Import SubstTypeType.
Require Import SubstTypeExp.
Require Import TyJudge.


(* Weakening Kind Env in TyJudge ************************************
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
  apply kind_kienv_insert. auto.

 Case "XLAM".
  eapply TYLAM. 
  rewrite insert_rewind. 
   rewrite (liftTE_liftTE 0 ix).
   apply IHx1. auto.

 Case "XAPP".
  rewrite (liftTT_substTT' 0 ix). simpl.
  eapply TYAPP.
  eapply (IHx1 ix) in H4. simpl in H4. eauto.
  apply kind_kienv_insert. auto.

 Case "XLam".
  apply TYLam.
   apply kind_kienv_insert. auto.
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
 ,  TYPE ke                 te            x1              t1
 -> TYPE (ke :> k2)        (liftTE 0 te) (liftTX 0 x1)   (liftTT 0 t1).
Proof.
 intros.
 assert (ke :> k2 = insert 0 k2 ke). 
  destruct ke; auto. rewrite H0.
  apply type_kienv_insert. auto.
Qed.


(* Weakening Type Env in TyJudge ************************************
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


(* Substitution of Values in Values preserves Typing ****************)
Theorem subst_value_value_ix
 :  forall ix ke te x1 t1 x2 t2
 ,  get  te ix = Some t2
 -> TYPE ke te           x1 t1
 -> TYPE ke (drop ix te) x2 t2
 -> TYPE ke (drop ix te) (substXX ix x2 x1) t1.
Proof.
 intros. gen ix ke te t1 x2 t2.
 induction x1; intros; inverts H0; simpl; eauto.

 Case "XVar".
  fbreak_nat_compare.
  SCase "n = ix".
   rewrite H in H3. inverts H3. auto.

  SCase "n < ix".
   apply TYVar. 
   rewrite <- H3. apply get_drop_above. auto. auto.

  SCase "n > ix".
   apply TYVar. auto.
   rewrite <- H3.
   destruct n.
    burn.
    simpl. nnat. apply get_drop_below. omega.
    auto.

 Case "XLAM".
  eapply (IHx1 ix) in H5.
  apply TYLAM.
   unfold liftTE. rewrite map_drop. eauto.
   eapply get_map. eauto.
   unfold liftTE. rewrite <- map_drop.
    assert (map (liftTT 0) (drop ix te) = liftTE 0 (drop ix te)). 
     unfold liftTE. auto. rewrite H0. clear H0.
    apply type_kienv_weaken. auto.

 Case "XLam".
  apply TYLam.
   auto.
   rewrite drop_rewind.
   eapply IHx1; eauto.
   simpl. apply type_tyenv_weaken. auto.
Qed.


Theorem subst_value_value
 :  forall ke te x1 t1 x2 t2
 ,  TYPE ke (te :> t2) x1 t1
 -> TYPE ke te x2 t2
 -> TYPE ke te (substXX 0 x2 x1) t1.
Proof.
 intros.
 assert (te = drop 0 (te :> t2)). auto.
 rewrite H1. eapply subst_value_value_ix; eauto. eauto.
Qed.





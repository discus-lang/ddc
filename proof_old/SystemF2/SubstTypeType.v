
Require Import KiJudge.


(* Weakening Kind environment ***************************************)
Lemma kind_kienv_insert
 :  forall ke ix t k1 k2
 ,  KIND ke t k1
 -> KIND (insert ix k2 ke) (liftTT ix t) k1.
Proof.
 intros. gen ix ke k1.
 induction t; intros; simpl; inverts H; eauto.

 Case "TVar".
  lift_cases; intros; auto.

 Case "TForall".
  apply KIForall.
  rewrite insert_rewind.
  apply IHt. auto.
Qed.


Lemma kind_kienv_weaken
 :  forall ke t k1 k2
 ,  KIND  ke         t           k1
 -> KIND (ke :> k2) (liftTT 0 t) k1.
Proof.
 intros.
 assert (ke :> k2 = insert 0 k2 ke). simpl.
   destruct ke; auto.
 rewrite H0. apply kind_kienv_insert. auto.
Qed.


(* Theorems *********************************************************)
(* Substitution of types in types preserves kinding.
   Must also subst new new type into types in env higher than ix
   otherwise indices ref subst type are broken.
   Resulting type env would not be well formed *)

Theorem subst_type_type_ix
 :  forall ix ke t1 k1 t2 k2
 ,  get ke ix = Some k2
 -> KIND ke t1 k1
 -> KIND (drop ix ke) t2 k2
 -> KIND (drop ix ke) (substTT ix t2 t1) k1.
Proof.
 intros. gen ix ke t2 k1 k2.
 induction t1; intros; simpl; inverts H0; eauto.

 Case "TVar".
  fbreak_nat_compare.
  SCase "n = ix".
   rewrite H in H4.
   inverts H4. auto.

  SCase "n < ix".
   apply KIVar. rewrite <- H4.
   apply get_drop_above; auto.

  SCase "n > ix".
   apply KIVar. rewrite <- H4.
   destruct n.
    burn.
    simpl. nnat. apply get_drop_below. omega.

 Case "TForall".
  apply KIForall.
  rewrite drop_rewind.
  eapply IHt1; eauto.
   apply kind_kienv_weaken. auto.
Qed.


Theorem subst_type_type
 :  forall ke t1 k1 t2 k2
 ,  KIND (ke :> k2) t1 k1
 -> KIND ke         t2 k2
 -> KIND ke (substTT 0 t2 t1) k1.
Proof.
 intros.
 unfold substTT.
 assert (ke = drop 0 (ke :> k2)). auto. rewrite H1.
 eapply subst_type_type_ix; simpl; eauto.
Qed.


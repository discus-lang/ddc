
Require Import TyJudge.
Require Export Exp.

(* Substitution of values in values. 
   Inductively, we must reason about performing substitutions at any
   depth, hence we must prove a property about (subst' d x2 x1) instead
   of the weaker (subst x2 x1) which assumes the substitution is taking
   place at top level.
 *)
Theorem subst_value_value_ix
 :  forall ix ds te x1 x2 t1 t2
 ,  get  te ix = Some t2
 -> TYPE ds te           x1 t1
 -> TYPE ds (drop ix te) x2 t2
 -> TYPE ds (drop ix te) (substX ix x2 x1) t1.
Proof.
 intros. gen ix ds te x2 t1 t2.
 induction x1 using exp_mutind with 
  (PA := fun a1 => forall ix ds te x2 t11 t12 t2
      ,  get te ix = Some t2
      -> TYPEA ds te           a1 t11 t12
      -> TYPE  ds (drop ix te) x2 t2
      -> TYPEA ds (drop ix te) (substA ix x2 a1) t11 t12)
  ; intros; simpl.

 Case "XVar".
  inverts H0.
  fbreak_nat_compare.
  SCase "i = ix".
   rewrite H in H5. inverts H5. auto.

  SCase "n < ix".
   apply TYVar.
   rewrite <- H5.
    apply get_drop_above. auto.

  SCase "n > ix".
   apply TYVar.
   destruct n.
    false. omega.
    simpl. nnat. rewrite <- H5.
     apply get_drop_below. omega.

 Case "XLam".
  inverts H0.
  apply TYLam.
  rewrite drop_rewind.
  eapply IHx1; eauto.
   simpl. apply type_tyenv_weaken1. auto.

 Case "XApp". 
  inverts H0. eauto.

 Case "XCon".
  inverts H0.
  eapply TYCon; eauto.
   rewrite Forall_forall in H.
   apply (Forall2_map_left (TYPE ds (drop ix te))).
   apply (Forall2_impl_In  (TYPE ds te)); eauto.

 Case "XCase".
  inverts H0.
  eapply TYCase.
   eauto. clear IHx1.
   rewrite Forall_forall in H.
   eapply Forall_map.
   eapply (Forall_impl_In 
    (fun a => TYPEA ds te a (TCon tcPat) t1)); eauto.
   rewrite map_length. auto. eauto.

   eapply Forall_impl; eauto.
    intros. simpl in H0.
    rewrite map_map.
    apply in_map_iff.
    apply in_map_iff in H0.
     destruct H0. exists x. inverts H0.
     split; auto.
     rewrite dcOfAlt_substA. auto.
     
 Case "AAlt".
  inverts H0.
  eapply TYAlt. 
   eauto.
   rewrite drop_append.
    rewrite <- length_envOfList.
    eapply IHx1 with (t2 := t2).
     auto.
     rewrite <- (get_append_right_some ty ix te (envOfList ts)). 
      auto. auto.
     rewrite <- drop_append.
      apply type_tyenv_weaken_append. eauto.
Qed.


Theorem subst_value_value
 :  forall ds te x1 x2 t1 t2
 ,  TYPE ds (te :> t2) x1 t1
 -> TYPE ds te         x2 t2 
 -> TYPE ds te (substX 0 x2 x1) t1.
Proof. 
 intros ds te x1 x2 t1 t2 Ht1 Ht2.
 lets H: subst_value_value_ix 0 (te :> t2).
  simpl in H. eauto.
Qed.


(* Substitution of several expressions at once. *)
Theorem subst_value_value_list
 :  forall ds te x1 xs t1 ts
 ,  Forall2 (TYPE ds te)         xs ts
 -> TYPE ds (te ++ envOfList ts) x1 t1
 -> TYPE ds te    (substXs 0 xs x1) t1.
Proof.
 intros ds te x1 xs t1 ts HF HT.
 gen ts x1.
 induction xs; intros.

 Case "base case".
  destruct ts.
   simpl. simpl in HT. auto.
  inverts HF.

 Case "step case".
  simpl. 
   destruct ts.
    inverts HF.
    inverts HF.
    eapply IHxs. eauto.
    simpl in HT.
    eapply subst_value_value. eauto.
    assert (List.length xs = List.length ts).
     eapply Forall2_length. eauto.
    rewrite H. clear H.
    rewrite <- length_envOfList.
    eapply type_tyenv_weaken_append.
    auto.
Qed.


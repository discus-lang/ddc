
Require Import DDC.Language.SystemF.KiJudge.


(* Substitution of types in types preserves kinding. *)
Theorem subst_type_type_ix
 :  forall ix ke t1 k1 t2 k2
 ,  get ix ke = Some k2
 -> KIND ke t1 k1
 -> KIND (delete ix ke) t2 k2
 -> KIND (delete ix ke) (substTT ix t2 t1) k1.
Proof.
 intros. gen ix ke t2 k1 k2.
 induction t1; intros; simpl; inverts H0; eauto.

 Case "TVar".
  destruct k1. destruct k2.
  fbreak_nat_compare.
  SCase "n = ix".
   auto.

  SCase "n < ix".
   apply KIVar. rewrite <- H4.
   apply get_delete_above; auto.

  SCase "n > ix".
   apply KIVar. rewrite <- H4.
   destruct n.
    burn.
    simpl. nnat. apply get_delete_below. omega.

 Case "TForall".
  apply KIForall.
  rewrite delete_rewind.
  eapply IHt1; eauto.
   apply liftTT_weaken. auto.
Qed.


Theorem subst_type_type
 :  forall ke t1 k1 t2 k2
 ,  KIND (ke :> k2) t1 k1
 -> KIND ke         t2 k2
 -> KIND ke (substTT 0 t2 t1) k1.
Proof.
 intros.
 unfold substTT.
 assert (ke = delete 0 (ke :> k2)). auto. rewrite H1.
 eapply subst_type_type_ix; simpl; eauto.
Qed.


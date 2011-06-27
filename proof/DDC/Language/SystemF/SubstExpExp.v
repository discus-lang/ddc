
Require Import DDC.Language.SystemF.SubstTypeType.
Require Import DDC.Language.SystemF.SubstTypeExp.
Require Import DDC.Language.SystemF.TyJudge.


(* Substitution of Values in Values preserves Typing ****************)
Theorem subst_value_value_ix
 :  forall ix ke te x1 t1 x2 t2
 ,  get  ix te = Some t2
 -> TYPE ke te           x1 t1
 -> TYPE ke (delete ix te) x2 t2
 -> TYPE ke (delete ix te) (substXX ix x2 x1) t1.
Proof.
 intros. gen ix ke te t1 x2 t2.
 induction x1; intros; inverts H0; simpl; eauto.

 Case "XVar".
  fbreak_nat_compare.
  SCase "n = ix".
   rewrite H in H3. inverts H3. auto.

  SCase "n < ix".
   apply TYVar. 
   rewrite <- H3. apply get_delete_above. auto. auto.

  SCase "n > ix".
   apply TYVar. auto.
   rewrite <- H3.
   destruct n.
    burn.
    simpl. nnat. apply get_delete_below. omega.
    auto.

 Case "XLAM".
  eapply (IHx1 ix) in H5.
  apply TYLAM.
   unfold liftTE. rewrite map_delete. eauto.
   eapply get_map. eauto.
   unfold liftTE. rewrite <- map_delete.
    assert (map (liftTT 0) (delete ix te) = liftTE 0 (delete ix te)). 
     unfold liftTE. auto. rewrite H0. clear H0.
    apply type_kienv_weaken. auto.

 Case "XLam".
  apply TYLam.
   auto.
   rewrite delete_rewind.
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
 assert (te = delete 0 (te :> t2)). auto.
 rewrite H1. eapply subst_value_value_ix; eauto. eauto.
Qed.


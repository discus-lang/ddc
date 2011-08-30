
Require Import DDC.Language.SystemF2.SubstTypeType.
Require Import DDC.Language.SystemF2Data.SubstTypeExp.
Require Import DDC.Language.SystemF2Data.TyJudge.


(* Substitution of exps in exps preserves typing *)
Theorem subst_exp_exp_ix
 :  forall ix ds ke te x1 t1 x2 t2
 ,  get  ix te = Some t2
 -> TYPE ds ke te           x1 t1
 -> TYPE ds ke (delete ix te) x2 t2
 -> TYPE ds ke (delete ix te) (substXX ix x2 x1) t1.
Proof.
 intros. gen ix ds ke te t1 x2 t2.
 induction x1 using exp_mutind with 
  (PA := fun a1 => forall ix ds ke te x2 t11 t12 t2
      ,  get ix te = Some t2
      -> TYPEA ds ke te           a1 t11 t12
      -> TYPE  ds ke (delete ix te) x2 t2
      -> TYPEA ds ke (delete ix te) (substXA ix x2 a1) t11 t12)
  ; intros; simpl; inverts_type; eauto.

 Case "XVar".
  fbreak_nat_compare.
  SCase "n = ix".
   rewrite H in H3. inverts H3. auto.

  SCase "n < ix".
   apply TYVar; auto.

  SCase "n > ix".
   apply TYVar; auto.
   rewrite <- H3.
   destruct n.
    burn. 
    simpl. nnat. apply get_delete_below; burn.

 Case "XLAM".
  simpl.
  eapply (IHx1 ix) in H3.
  apply TYLAM.
   unfold liftTE. rewrite map_delete. eauto.
   eapply get_map. eauto.
   unfold liftTE. rewrite <- map_delete.
    rrwrite (map (liftTT 1 0) (delete ix te) = liftTE 0 (delete ix te)). 
    apply type_kienv_weaken1. auto.

 Case "XLam".
  simpl.
  apply TYLam; auto.
   rewrite delete_rewind.
   eauto using type_tyenv_weaken1.

 Case "XCon".
  eapply TYCon; eauto.
   nforall.
   apply (Forall2_map_left (TYPE ds ke (delete ix te))).
   apply (Forall2_impl_in  (TYPE ds ke te)); eauto.

 Case "XCase".
  eapply TYCase; eauto.
   clear IHx1.
   (* Alts have correct type *)
    eapply Forall_map.
    nforall. eauto.

   (* Required datacon is in alts list *)
   nforall. intros.
   rename x into d.
   rewrite map_map. unfold compose.
   apply in_map_iff.
   assert (exists a, dcOfAlt a = d /\ In a aa). 
    eapply map_in_exists. eauto. shift a. int.
   rewrite <- H4. 
   rewrite dcOfAlt_substXA; auto.

 Case "AAlt".
  (* TODO: tactic for this *)
  assert (DEFOK ds (DefData dc tsFields tc)).
   eauto. inverts H0. rewrite H9 in H5. inverts H5. subst.

  eapply TYAlt; eauto.
  rewrite delete_app.
  lists.
  assert ( length tsFields 
         = length (map (substTTs 0 tsParam) tsFields)) as HL.
   lists. auto.
  rewrite HL.

  eapply IHx1 with (t2 := t2); eauto.
  rewrite <- delete_app.
  eauto using type_tyenv_weaken_append.
Qed.


Theorem subst_exp_exp
 :  forall ds ke te x1 t1 x2 t2
 ,  TYPE ds ke (te :> t2) x1 t1
 -> TYPE ds ke te x2 t2
 -> TYPE ds ke te (substXX 0 x2 x1) t1.
Proof.
 intros.
 rrwrite (te = delete 0 (te :> t2)). 
 eapply subst_exp_exp_ix; burn.
Qed.


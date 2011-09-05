Require Import DDC.Language.SystemF2.SubstTypeType.
Require Import DDC.Language.SystemF2Data.TyJudge.
Require Import Coq.Logic.FunctionalExtensionality.


Theorem subst_type_exp_ix
 :  forall ix ds ke te x1 t1 t2 k2
 ,  get ix ke = Some k2
 -> TYPE ds ke  te x1 t1
 -> KIND (delete ix ke)  t2 k2
 -> TYPE ds (delete ix ke)     (substTE ix t2 te)
            (substTX ix t2 x1) (substTT ix t2 t1).
Proof.
 intros. gen ix ds ke te t1 t2 k2.
 induction x1 using exp_mutind with 
  (PA := fun a => forall ix ds ke te t1 t2 t3 k3
      ,  get ix ke = Some k3
      -> TYPEA ds ke te a t1 t2
      -> KIND (delete ix ke) t3 k3
      -> TYPEA ds (delete ix ke)  (substTE ix t3 te) (substTA ix t3 a)
                  (substTT ix t3 t1) (substTT ix t3 t2));
  intros; simpl; inverts_type; eauto.

 Case "XVar".
  apply TYVar.
  unfold substTE. auto. 

 Case "XLAM".
  simpl. apply TYLAM.
  rewrite delete_rewind.
  rewrite (liftTE_substTE 0 ix).
  eauto using kind_kienv_weaken.

 Case "XAPP".
  rewrite (substTT_substTT 0 ix).
  eauto using subst_type_type_ix.
  apply TYAPP.
   simpl. eapply (IHx1 ix) in H4; eauto.
   simpl. eapply subst_type_type_ix; eauto.

 Case "XLam".
  simpl. apply TYLam.
  eapply subst_type_type_ix; eauto.
  unfold substTE. rewrite map_rewind.
  rrwrite ( map (substTT ix t2) (te :> t)
          = substTE ix t2 (te :> t)).
  burn.

 Case "XApp".
  eapply TYApp.
   eapply IHx1_1 in H4; eauto.
    simpl in H4. burn.
   eapply IHx1_2 in H6; eauto.

 Case "XCon".
  rr. simpl.
  eapply TYCon; eauto.
   eapply subst_type_type_ix_forall2; eauto.

   eapply Forall2_map.
   eapply Forall2_map_right' in H11.
   eapply Forall2_impl_in; eauto; intros.
    rrwrite (ix = 0 + ix). 
    rewrite substTTs_substTT; rr.
     nforall. eapply H; eauto.
     defok ds (DefData     dc tsFields tc).
     defok ds (DefDataType tc ks       dcs).
     rrwrite (length ts = length ks).
     nforall.
     have (KIND ks y KStar). eauto.
 
 Case "XCase".
  eapply TYCase; eauto.
  eapply Forall_map.
  nforall. intros. eauto.
  nforall. intros.
   have (In x (map dcOfAlt aa)).
   assert ( map dcOfAlt (map (substTA ix t2) aa)
          = map dcOfAlt aa) as HDC.
    lists. f_equal.
    extensionality x0. rr. auto.
   rewrite HDC. auto.

 Case "AAlt".
  defok ds (DefData dc tsFields tc).
  rr.
  eapply TYAlt with (tc := tc) (ks := ks) (dcs := dcs); eauto.
  eapply subst_type_type_ix_forall2; eauto.
   eapply IHx1 in H10; eauto.
   rrwrite (ix = 0 + ix).   
   rewrite substTTs_substTT_map; rr.
    unfold substTE. rewrite <- map_app. auto. 
    rrwrite (length tsParam = length ks).
    eauto.
Qed.


Theorem subst_type_exp
 :  forall ds ke te x1 t1 t2 k2
 ,  TYPE ds (ke :> k2) te x1 t1
 -> KIND ke  t2 k2
 -> TYPE ds ke (substTE 0 t2 te) (substTX 0 t2 x1) (substTT 0 t2 t1).
Proof.
 intros. 
 rrwrite (ke = delete 0 (ke :> k2)).
 eapply subst_type_exp_ix; burn.
Qed.


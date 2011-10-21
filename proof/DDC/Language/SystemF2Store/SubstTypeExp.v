Require Import DDC.Language.SystemF2.SubstTypeType.
Require Import DDC.Language.SystemF2Store.TyJudge.
Require Import Coq.Logic.FunctionalExtensionality.


Theorem subst_type_exp_ix
 :  forall ix ds ke te se x1 t1 t2 k2
 ,  get ix ke = Some k2
 -> TYPE ds ke  te se x1 t1
 -> KIND (delete ix ke)  t2 k2
 -> TYPE ds (delete ix ke)     (substTE ix t2 te)  (substTE ix t2 se)
            (substTX ix t2 x1) (substTT ix t2 t1).
Proof.
 intros. gen ix ds ke te se t1 t2 k2.
 induction x1 using exp_mutind with 
  (PA := fun a => forall ix ds ke te se t1 t2 t3 k3
      ,  get ix ke = Some k3
      -> TYPEA ds ke te se a t1 t2
      -> KIND (delete ix ke) t3 k3
      -> TYPEA ds (delete ix ke)  (substTE ix t3 te) (substTE ix t3 se)
                  (substTA ix t3 a)
                  (substTT ix t3 t1) (substTT ix t3 t2));
  intros; simpl; inverts_type; eauto.

 Case "XVar".
  apply TyVar.
  unfold substTE. auto.

 Case "XLoc".
  eapply TyLoc;
   unfold substTE; eauto.

 Case "XLAM".
  simpl. apply TyLAM.
  rewrite delete_rewind.
  rewrite (liftTE_substTE 0 ix).
  rewrite (liftTE_substTE 0 ix).
  eauto using kind_kienv_weaken.

 Case "XAPP".
  rewrite (substTT_substTT 0 ix).
  eauto using subst_type_type_ix.
  apply TyAPP.
   simpl. eapply (IHx1 ix) in H4; eauto.
   simpl. eapply subst_type_type_ix; eauto.

 Case "XLam".
  simpl. apply TyLam.
  eapply subst_type_type_ix; eauto.
  unfold substTE at 1. rewrite map_rewind.
  rrwrite ( map (substTT ix t2) (te :> t)
          = substTE ix t2 (te :> t)).
  eauto.

 Case "XApp".
  eapply TyApp.
   eapply IHx1_1 in H4; eauto.
    simpl in H4. burn.
   eapply IHx1_2 in H6; eauto.

 Case "XCon".
  rr. simpl.
  eapply TyCon; eauto.
   eapply subst_type_type_ix_forall2; eauto.

   eapply Forall2_map.
   eapply Forall2_map_right' in H11.
   eapply Forall2_impl_in; eauto; intros.
    rrwrite (ix = 0 + ix). 
    rewrite substTTs_substTT; rr.
     nforall. eapply H; eauto.
     defok ds (DefData dc tsFields tc).
     defok ds (DefType tc ks       dcs).
     rrwrite (length ts = length ks).
     nforall.
     have (KIND ks y KStar). eauto.
 
 Case "XCase".
  eapply TyCase; eauto.
  eapply Forall_map.
  nforall. intros. eauto.
  nforall. intros.
   have (In x (map dcOfAlt aa)).
   assert ( map dcOfAlt (map (substTA ix t2) aa)
          = map dcOfAlt aa) as HDC.
    lists. f_equal.
    extensionality x0. rr. auto.
   rewrite HDC. auto.

 Case "XUpdate".
  defok ds (DefData dc    tsFields tcObj).
  defok ds (DefType tcObj ks dcs).
  eapply TyUpdate; eauto.
  eapply Forall2_map_left.
   eapply Forall2_impl; eauto; intros.
   eapply subst_type_type_ix; eauto.
  rrwrite (TCon tcObj = substTT ix t2 (TCon tcObj)).
  rewrite <- substTT_makeTApps.
  eauto.
  rrwrite (ix = 0 + ix).
  rewrite substTTs_substTT; eauto.
  rr.
  rrwrite (length ts = length ks).
  nforall. eauto.

 Case "AAlt".
  defok ds (DefData dc tsFields tc).
  rr.
  eapply TyAlt with (tc := tc) (ks := ks) (dcs := dcs); eauto.
  eapply subst_type_type_ix_forall2; eauto.
   eapply IHx1 in H10; eauto.
   rrwrite (ix = 0 + ix).   
   rewrite substTTs_substTT_map; rr.
    unfold substTE. rewrite <- map_app. auto. 
    rrwrite (length tsParam = length ks).
    eauto.
Qed.


Theorem subst_type_exp
 :  forall ds ke te se x1 t1 t2 k2
 ,  TYPE ds (ke :> k2) te se x1 t1
 -> KIND ke  t2 k2
 -> TYPE ds ke (substTE 0 t2 te) (substTE 0 t2 se) 
               (substTX 0 t2 x1) (substTT 0 t2 t1).
Proof.
 intros. 
 rrwrite (ke = delete 0 (ke :> k2)).
 eapply subst_type_exp_ix; burn.
Qed.


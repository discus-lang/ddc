
Require Import DDC.Language.SystemF2Effect.SubstTypeType.
Require Import DDC.Language.SystemF2Effect.VaExpSubst.
Require Import DDC.Language.SystemF2Effect.TyEnv.
Require Import DDC.Language.SystemF2Effect.TyJudge.
Require Import Coq.Logic.FunctionalExtensionality.


Theorem subst_type_exp_ix
 :  forall ix ke te se x1 t1 e1 t2 k2
 ,  get ix ke = Some k2
 -> TYPEX ke te se x1 t1 e1
 -> KIND  (delete ix ke) t2 k2
 -> TYPEX (delete ix ke)     (substTE ix t2 te)  (substTE ix t2 se)
          (substTX ix t2 x1) (substTT ix t2 t1)  (substTT ix t2 e1).
Proof.
 intros. gen ix ke te se t1 t2 e1 k2.
 induction x1 using exp_mutind with 
  (PV := fun v => forall ix ke te se t1 t2 k3
      ,  get ix ke = Some k3
      -> TYPEV ke te se v t1
      -> KIND  (delete ix ke) t2 k3
      -> TYPEV (delete ix ke)   (substTE ix t2 te) (substTE ix t2 se)
               (substTV ix t2 v)(substTT ix t2 t1));
  intros; simpl; inverts_type; eauto.

 Case "VVar".
  apply TvVar; auto.
  unfold substTE. auto.

 Case "VLoc".
  eapply TvLoc;
   unfold substTE; eauto.

 Case "VLam".
  simpl. apply TvLam.
  eapply subst_type_type_ix; eauto.
  unfold substTE at 1. rewrite map_rewind.
  rrwrite ( map (substTT ix t2) (te :> t)
          = substTE ix t2 (te :> t)).
  eauto.

 Case "VLAM".
  simpl. apply TvLAM.
  rewrite delete_rewind.
  rewrite (liftTE_substTE 0 ix).
  rewrite (liftTE_substTE 0 ix).
  rrwrite ( TBot KEffect 
          = substTT (S ix) (liftTT 1 0 t2) (TBot KEffect)).
  eauto using kind_kienv_weaken.

 Case "XLet".
  simpl. apply TxLet.
   eapply subst_type_type_ix; eauto.
   eauto.
   unfold substTE at 1. rewrite map_rewind.
    rrwrite ( map (substTT ix t2) (te :> t)
            = substTE ix t2 (te :> t)).
   eauto.

 Case "XApp".
  eapply TxApp.
   eapply IHx1 in H7; eauto.
    simpl in H7. burn.
   eapply IHx0 in H10; eauto.

 Case "XAPP".
  rrwrite ( TBot KEffect
          = substTT 0 t (TBot KEffect)).
  rewrite (substTT_substTT 0 ix).
  rewrite (substTT_substTT 0 ix).
  eapply TvAPP.
   simpl. eapply (IHx1 ix) in H7; eauto.
   simpl. eauto using subst_type_type_ix.

 Case "XAlloc".
  eapply TxOpAlloc; fold substTT.
   eauto using subst_type_type_ix.
   eauto.

 Case "XRead".
  eapply TxOpRead; fold substTT.
  rrwrite ( tRef (substTT ix t2 r1) (substTT ix t2 t1)
          = substTT ix t2 (tRef r1 t1)).
  eauto.

 Case "XWrite".
  eapply TxOpWrite; fold substTT.
   eapply IHx1 in H7;  eauto. simpl in H7. eauto.
   eapply IHx0 in H10; eauto.

 Case "OSucc".
  eapply TxOpSucc.
  rrwrite (tNat = substTT ix t2 tNat). eauto.

 Case "OIsZero".
  eapply TxOpIsZero.
  rrwrite (tNat = substTT ix t2 tNat). eauto.
Qed.


Theorem subst_type_exp
 :  forall ke te se x1 t1 e1 t2 k2
 ,  TYPEX (ke :> k2) te se x1 t1 e1
 -> KIND  ke  t2 k2
 -> TYPEX ke (substTE 0 t2 te) (substTE 0 t2 se) 
             (substTX 0 t2 x1) (substTT 0 t2 t1) (substTT 0 t2 e1).
Proof.
 intros. 
 rrwrite (ke = delete 0 (ke :> k2)).
 eapply subst_type_exp_ix; burn.
Qed.


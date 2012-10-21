
Require Import DDC.Language.SystemF2Effect.SubstTypeType.
Require Import DDC.Language.SystemF2Effect.SubstTypeExp.
Require Import DDC.Language.SystemF2Effect.TyJudge.
Require Import DDC.Language.SystemF2Effect.TyEnv.
Require Import DDC.Language.SystemF2Effect.VaExpSubst.


(* Substitution of values in exps preserves typing *)
Theorem subst_val_exp_ix
 :  forall ix ke te se x1 t1 e1 v2 t2
 ,  get  ix te = Some t2
 -> TYPEX ke te             se x1 t1 e1
 -> TYPEV ke (delete ix te) se v2 t2
 -> TYPEX ke (delete ix te) se (substVX ix v2 x1) t1 e1.
Proof.
 intros. gen ix ke te se t1 e1 v2 t2.
 induction x1 using exp_mutind with 
  (PV := fun v1 => forall ix ke te se v2 t1 t2
      ,  get ix te = Some t2
      -> TYPEV ke te             se v1 t1
      -> TYPEV ke (delete ix te) se v2 t2
      -> TYPEV ke (delete ix te) se (substVV ix v2 v1) t1)
  ; intros; simpl; inverts_type; eauto.

 Case "VVar".
  fbreak_nat_compare.
  SCase "n = ix".
   have (t2 = t1) by congruence. subst.
   auto.

  SCase "n < ix".
   apply TvVar; auto.

  SCase "n > ix".
   apply TvVar; auto.
   rewrite <- H6.
   destruct n.
    burn. 
    simpl. nnat. apply get_delete_below; burn.

 Case "VLam".
  apply TvLam; auto.
   rewrite delete_rewind.
   eauto using typev_tyenv_weaken1.

 Case "VLAM".
  simpl.
  eapply (IHx1 ix) in H8.
  apply TvLAM.
   unfold liftTE. rewrite map_delete. eauto.
   eapply get_map. eauto.
   unfold liftTE. rewrite <- map_delete.
    rrwrite (map (liftTT 1 0) (delete ix te) = liftTE 0 (delete ix te)). 
    eauto using typev_kienv_weaken1.

 Case "XLet".
  apply TxLet; eauto.
   rewrite delete_rewind.
   eauto using typev_tyenv_weaken1.
Qed.


Theorem subst_val_exp
 :  forall ke te se x1 t1 e1 v2 t2
 ,  TYPEX  ke (te :> t2) se x1                t1 e1
 -> TYPEV  ke te se         v2                t2
 -> TYPEX  ke te se         (substVX 0 v2 x1) t1 e1.
Proof.
 intros.
 rrwrite (te = delete 0 (te :> t2)). 
 eapply subst_val_exp_ix; burn.
Qed.

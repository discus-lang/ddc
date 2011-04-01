
Require Import KiJudge.
Require Import TyJudge.
Require Import Program.

Definition substTTE a T2 tenv
 := map (substTT a T2) tenv.

(* Substitution of types in values preserves typing.
 *)
Lemma subst_type_value
 :  forall kenv tenv a t1 T1 T2 K2
 ,  kenv a = None
 -> (forall z, freeT z T2 -> ~bindsX z t1)
 -> TYPE (extend kenv a K2) tenv  t1  T1
 -> KIND      kenv                T2  K2
 -> TYPE kenv
         (substTTE a T2 tenv)
         (substTX  a T2 t1)
         (substTT  a T2 T1).  
Proof.
 intros kenv tenv a t1 T1 T2 K2.
 gen kenv tenv T1.
 induction t1; intros; inversions H1.

 Case "XVar".
  apply TYVar; auto.
  unfold substTTE. unfold map. break (tenv n).
  inversions H7. auto. false.

 Case "XLam". simpl.
  eapply TYLam. auto.
  eapply type_tyenv_invariance.
   apply IHt1; eauto.
    intros. apply H0 in H1; eauto.
    eauto.
    intros. unfold extend. break (beq_name n x).
     apply true_name_eq in HeqX. subst.
      unfold substTTE. unfold map.
      rewrite <- beq_name_refl. auto.
     apply false_name_neq in HeqX.
      unfold substTTE. unfold map.
      rewrite <- neq_name_false. auto. auto. 

 Case "XApp". simpl.
  eapply TYApp.
   eapply IHt1_1 in H7; eauto.
    intros. apply H0 in H1; eauto. fold substTT.
   eapply IHt1_2 in H9; eauto.
    intros. apply H0 in H1; eauto.

 Case "XLAM". simpl.
  break (beq_name a n).
  SCase "a = n".
   apply true_name_eq in HeqX. subst.
   rewrite -> extend_pack in H9.
   apply TYLAM; eauto.
   eapply type_tyenv_invariance. eauto. eauto.
   intros. unfold substTTE. sort. 
   admit. (* kenv n = None implies n not free in T2 *)

  SCase "a <> n".
   apply false_name_neq in HeqX.
   apply TYLAM; eauto.
   apply IHt1.
    rewrite extend_neq; auto.
    intros.
    apply H0 in H1. auto.
    rewrite extend_swap; eauto.
    admit. (* require T2 closed in theorem *)

 Case "XAPP". simpl.
  lets Ht1: IHt1 H7 H2.
   eauto.
   intros. specialize H0 with z. apply H0 in H1. contradict H1. eauto.
   assert (T2 = substTT a T2 t). admit. (* T2 closed *)
   rewrite <- H1. clear H1.
   eapply TYAPP.
    simpl in Ht1.
     remember (beq_name a a0) in Ht1. destruct b.
      apply true_name_eq in Heqb. subst. eauto. admit. (* fix *)

   admit.
   destruct K2. auto.
Qed.
      



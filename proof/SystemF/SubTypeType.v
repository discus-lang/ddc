
Require Export TyJudge.
Require Export KiJudge.

(* Substitution of types in types preserves kinding *)
Lemma subst_type_type
 :  forall kenv a T1 T2 K1 K2
 ,  tyname a -> ~bindsT a T1 -> closedT T2
 -> KIND   (extend kenv a K2) T1 K1
 -> KIND                kenv  T2 K2
 -> KIND  kenv (substTT a T2 T1) K1.
Proof.
 intros.
 gen kenv K1.
 induction T1; intros.

 Case "TCon".
  simpl. inversions H2. eauto.

 Case "TVar".
  simpl. break (beq_name a n).
  SCase "a = n".
   apply true_name_eq in HeqX. subst.
   inversions H2. destruct K2. eauto. 
  SCase "a <> n".
   apply false_name_neq in HeqX. 
   inversions H2. destruct K2. 
   rewrite extend_neq in H7. eauto. eauto.

 Case "TForall".
  inversions H2.
  simpl. break (beq_name a n).
  SCase "a = n".
   apply true_name_eq in HeqX. subst.
   contradict H0. eauto.
  SCase "a <> n".
   apply false_name_neq in HeqX.
   apply KIForall. auto.
   assert (~bindsT a T1). eauto. 

   lets D: IHT1 H2.
   specialize D with (kenv := extend kenv n K2).
   destruct K2. apply D.
    eapply kind_kienv_invariance.
    eauto. intros. contradict H5. eauto.
    
   rewrite extend_swap. eauto. eauto.

 Case "TFun".
  inversions H2.
  apply KIFun; fold substTT.
   eapply IHT1_1; eauto.
   eapply IHT1_2; eauto.
Qed.



Require Import KiJudge.
Require Import TyJudge.
Require Import Program.
Require Import SubTypeType.

Definition substTTE a T2 tenv
 := map (substTT a T2) tenv.


(* Substitution of types in values preserves typing.
 *)
Lemma subst_type_value
 :  forall kenv tenv a t1 T1 T2 K2
 ,  tyname a -> closedT T2 -> ~bindsX a t1
 -> TYPE (extend kenv a K2) tenv  t1  T1
 -> KIND      kenv                T2  K2
 -> TYPE kenv
         (substTTE a T2 tenv)
         (substTX  a T2 t1)
         (substTT  a T2 T1).  
Proof.
 intros kenv tenv a t1 T1 T2 K2.
 intros Htn Hc Hnb.
 gen kenv tenv T1.
 induction t1; intros.

 Case "XVar".
  inversions H.
  apply TYVar; auto.
  unfold substTTE. unfold map. break (tenv n).
  inversions H5. auto. false.

 Case "XLam".
  inversions H. simpl.
  eapply TYLam. auto.
  eapply type_tyenv_invariance. 
   apply IHt1.
    eauto. eauto. eauto.
    intros. eauto.
    intros. unfold substTTE. unfold extend. unfold map.
    break (beq_name n x).
     eauto.
     apply false_name_neq in HeqX. eauto.

 Case "XApp". simpl.
  inversions H.
  eapply TYApp.
   eapply IHt1_1 in H5; eauto. fold substTT.
   eapply IHt1_2 in H7; eauto.

 Case "XLAM".
  inversions H. simpl. 
  break (beq_name a n).
  SCase "a = n".
   apply true_name_eq in HeqX. symmetry in HeqX. subst.
   contradict Hnb. auto.

  SCase "a <> n".
   apply false_name_neq in HeqX.
   apply TYLAM; eauto.
   apply IHt1. eauto.
    rewrite extend_swap. eauto. eauto.
    eapply kind_kienv_invariance. eauto.
     eauto. intros.
     contradict H1. unfold closedT in Hc.
     eauto.

 Case "XAPP". simpl.
  inverts keep H. 
  assert (~bindsX a t1).
   intro. contradict Hnb. eauto.
  destruct K2.
  rename t into T3. 
  lets Ht1: IHt1 H1 H6 H0. sort.
  simpl in Ht1.
   breaka (beq_name a a0). 
   SCase "a = a0". 
    apply true_name_eq in HeqX. subst.
    admit. (* add T2 no binds a0 *)

   SCase "a <> a0".
    apply false_name_neq in HeqX.
    assert (~bindsT a T3). eauto.
    lets Hk1: subst_type_type Htn H8 H0; auto.
    lets Ht2: TYAPP Ht1 Hk1; auto.
    admit. (* TODO: finish this *)
Qed.











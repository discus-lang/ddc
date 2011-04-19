
Require Import Exp.
Require Import TyJudge.


(* Substitution of values in values preserves typing.
 *)
Lemma subst_value_value
 :  forall kenv tenv x t1 t2 T1 T2
 ,  valname x -> closedX t2
 -> TYPE kenv (extend tenv x T2)  t1  T1
 -> TYPE kenv tenv                t2  T2
 -> TYPE kenv tenv  (substXX x t2 t1) T1.
Proof.
 intros kenv tenv x t1 t2 T1 T2.
 gen kenv tenv T1.
 induction t1; intros.

 Case "XVar".
  simpl. rename n into y.
  break (beq_name x y).
  SCase "x=y".
   apply true_name_eq in HeqX. subst.
   inversions H1.
   rewrite extend_eq in H7. inversions H7. eauto.

  SCase "x<>y".
   apply false_name_neq in HeqX.
   inversions H1.
   rewrite extend_neq in H7. auto. auto.

 Case "XLam".
  rename n into y.
  simpl. break (beq_name x y).
  SCase "x=y".
   apply true_name_eq in HeqX. subst.
   inversions H1.
   rewrite extend_pack in H10. eauto.

  SCase "x<>y".
   apply false_name_neq in HeqX.
   inversions H1.
   apply TYLam. auto.
    apply IHt1; auto.
    rewrite extend_swap in H10. eauto. eauto.
    eapply type_tyenv_invariance. eauto. eauto.
     intros. false. unfold closedX in H0. 
     specialize H0 with x0. eauto.

 Case "XApp".
   simpl. inversions H1.
   eapply TYApp.
    eapply IHt1_1 in H7; eauto.
    eapply IHt1_2 in H9; eauto.

 Case "XLAM".
   simpl. inversions H1.
   eapply TYLAM. eauto.
   apply IHt1; eauto.
    eapply type_tyenv_invariance; eauto.
     intros. unfold closedX in H0.
     specialize H0 with a. eauto.

 Case "XAPP".
   simpl. inversions H1.
   apply TYAPP; eauto.
Qed.

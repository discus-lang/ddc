
Require Import TyJudge.


(* Substitution of values in values preserves typing.
 *)
Lemma subst_value_value
 :  forall kenv tenv x t1 t2 T1 T2
 ,  (forall z, freeX z t2 -> freshX z t1)
 -> TYPE kenv (extend tenv x T2)  t1  T1
 -> TYPE kenv tenv                t2  T2
 -> TYPE kenv tenv  (substXX x t2 t1) T1.
Proof.
 intros kenv tenv x t1 t2 T1 T2.
 generalize dependent kenv.
 generalize dependent tenv.
 generalize dependent T1.
 induction t1.

 Case "XVar".
  intros. simpl. rename n into y.
  remember (beq_name x y) as e. destruct e.
  SCase "x=y".
   apply true_name_eq in Heqe. subst.
   inversion H0. subst.
   rewrite extend_eq in H6. inversion H6. subst. eauto.
  SCase "x<>y".
   apply false_name_neq in Heqe.
   inversion H0. subst.
   rewrite extend_neq in H6.
   apply TYVar; eauto. auto.

 Case "XLam".
  intros. rename n into y.
  simpl. remember (beq_name x y) as e. destruct e.
  SCase "x=y".
   apply true_name_eq in Heqe. subst.
   eapply type_tyenv_invariance. eauto. intros. auto.
   intros. apply nocapture_lam in H3. auto. 
  SCase "x<>y".
   apply false_name_neq in Heqe.
   inversion H0. subst.
   apply TYLam. auto.
    apply IHt1.
    intros. apply H in H2. inversion H2. subst. auto.
    rewrite extend_swap; auto.
    eapply type_tyenv_invariance.
     eauto.
     intros. auto.
     intros. apply H in H3. inversion H3. subst.
      rewrite extend_neq; auto.

 Case "XApp".
   intros. simpl. inversion H0. subst.
   eapply TYApp.
   eapply IHt1_1 in H6. eauto. 
    intros. apply H in H2. inversion H2. subst. auto. auto.
   eapply IHt1_2 in H8. eauto.
    intros. apply H in H2. inversion H2. subst. auto. auto.

 Case "XLAM".
   intros. simpl. inversion H0. subst.
   eapply TYLAM. eauto.
   eapply type_tyenv_invariance.
   apply IHt1.
    intros. apply H in H2. inversion H2.
    eauto. 
    eapply type_tyenv_invariance.
    eauto. intros. apply H in H3. inversion H3.
    intros. auto.
    intros. auto.
    intros. auto.

 Case "XAPP".
   intros. simpl. inversion H0. subst.
   apply TYAPP.
   apply IHt1.
    intros. apply H in H2. inversion H2.
    auto.
    eapply type_tyenv_invariance. eauto.
     intros. auto.
     intros. auto. eauto.
Qed.

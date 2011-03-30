
Require Import Exp.
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
   inversions H0.
   rewrite extend_eq in H6. inversions H6. eauto.
  SCase "x<>y".
   apply false_name_neq in Heqe.
   inversions H0.
   rewrite extend_neq in H6.
   apply TYVar; eauto. auto.

 Case "XLam".
  intros. rename n into y.
  simpl. remember (beq_name x y) as e. destruct e.
  SCase "x=y".
   apply true_name_eq in Heqe. subst.
   eapply type_tyenv_invariance. eauto.
    intros. auto.
    intros. apply nocapture_lam in H3. auto. 
  SCase "x<>y".
   apply false_name_neq in Heqe.
   inversions H0.
   apply TYLam. auto.
    apply IHt1.
     intros. apply H in H0. inversions H0. auto.
    rewrite extend_swap; auto.
    eapply type_tyenv_invariance.
     eauto.
     auto.
     intros. apply H in H2. inversions H2. rewrite extend_neq; auto.

 Case "XApp".
   intros. simpl. inversions H0.
   eapply TYApp.
    eapply IHt1_1 in H6. eauto. 
     intros. apply H in H0. inversions H0. auto. auto.
    eapply IHt1_2 in H8. eauto.
     intros. apply H in H0. inversions H0. auto. auto.

 Case "XLAM".
   intros. simpl. inversions H0.
   eapply TYLAM. eauto.
   apply IHt1.
    intros. apply H in H0. inversion H0.
    auto. auto.
    eapply type_tyenv_invariance; eauto.
     intros. apply H in H2. inversions H2.
     rewrite extend_neq; auto.

 Case "XAPP".
   intros. simpl. inversions H0.
   apply TYAPP.
    apply IHt1.
     intros. apply H in H0. inversions H0.
     
     eapply type_tyenv_invariance; eauto.
     eauto.
Qed.

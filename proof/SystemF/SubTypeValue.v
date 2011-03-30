
Require Import KiJudge.
Require Import TyJudge.
Require Import Program.

Definition substTTE a T2 tenv
 := map (substTT a T2) tenv.

(* Substitution of types in values preserves typing.
 *)
Lemma subst_type_value
 :  forall kenv tenv a t1 T1 T2 K2
 ,  (forall z, freeT z T2 -> freshX z t1)
 -> TYPE (extend kenv a K2) tenv  t1  T1
 -> KIND      kenv                T2  K2
 -> TYPE kenv
         (substTTE a T2 tenv)
         (substTX  a T2 t1)
         (substTT  a T2 T1).  
Proof.
 intros kenv tenv a t1 T1 T2 K2.
 generalize dependent kenv.
 generalize dependent tenv.
 generalize dependent T1.
 induction t1; intros; inversions H0.

 Case "XVar".
  apply TYVar; auto.
  unfold substTTE. unfold map. remember (tenv n) as e. destruct e.
   inversions H6. inversions H6. auto.

 Case "XLam". simpl.
  eapply TYLam. auto.
  eapply type_tyenv_invariance.
   apply IHt1; eauto.
    intros. apply H in H0. inversions H0. auto.
    eauto.
    intros. unfold extend. remember (beq_name n x) as e. destruct e.
     apply true_name_eq in Heqe. subst.
      unfold substTTE. unfold map.
      rewrite <- beq_name_refl. auto.
     apply false_name_neq in Heqe.
      unfold substTTE. unfold map.
      rewrite <- neq_name_false. auto. auto. 

 Case "XApp". simpl.
  eapply TYApp.
   eapply IHt1_1 in H6. eauto.
    intros. apply H in H0. inversions H0. auto. auto. fold substTT.
   eapply IHt1_2 in H8. eauto.
    intros. apply H in H0. inversions H0. auto. auto.

 Case "XLAM". simpl.
  remember (beq_name a n) as e. destruct e.
  apply true_name_eq in Heqe. subst.
   rewrite -> extend_pack in H8.
   rewrite -> extend_swap in H8.

  apply TYLAM. auto.
   apply IHt1 in H8.


eauto.
  apply true_name_eq in Heqe. subst.
   eapply type_tyenv_invariance in H8. eauto.
   eauto.
   intros. unfold substTTE. unfold map. 
   apply equal_f.

unfold substTT.
   intros.
  eapply IHt1 in H1.
   eauto.





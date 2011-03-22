

Require Import TyJudge.

Example type_example1 
 : TYPE empty (XLam nA tA (XVar nA)) (TFun tA tA).
Proof. auto. Qed.


Example type_example2 
 : TYPE empty 
        (XLam nA tA 
         (XLam nB (TFun tA tA)
          (XApp (XVar nB) (XApp (XVar nB) (XVar nA)))))
        (TFun tA (TFun (TFun tA tA) tA)).
Proof. auto. 
 apply  TYLam. apply TYLam.
 eapply TYApp. apply TYVar. auto.
 eapply TYApp. apply TYVar. auto.
 eapply TYVar. auto.
Qed.


Example type_nonexample1
 : ~exists T 
 , TYPE empty 
        (XLam nA tA
         (XLam nB tB
          (XApp (XVar nA) (XVar nB))))
        T.
Proof. 
 intros C. destruct C.
 inversion H.  subst. clear H.
 inversion H5. subst. clear H5.
 inversion H4. subst. clear H4.
 inversion H2. subst. clear H2.
 inversion H5. subst. clear H5.
 unfold extend in H1. simpl in H1.
 inversion H1.
Qed.


Example type_nonexample2 
 : ~exists T
 , TYPE empty
        (XLam nA (TFun tA tA)
         (XLam nB tB
          (XApp (XVar nA) (XVar nB))))
        T.
Proof.
 intros C. destruct C.
 inversion H.  subst. clear H.
 inversion H5. subst. clear H5.
 inversion H4. subst. clear H4. 
 inversion H2. subst. clear H2. 
 inversion H5. subst. clear H5.
 unfold extend in H1. simpl in H1. inversion H1. subst. clear H1. 
 unfold extend in H2. simpl in H2. inversion H2.
Qed.

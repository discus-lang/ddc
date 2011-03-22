
Require Import Base.
Require Import Name.
Require Import Context.
Require Import Ty.
Require Import Exp.

Definition tyenv := partial_map ty.

Inductive TYPE : tyenv -> exp -> ty -> Prop :=
 | TYVar 
   :  forall env x T
   ,  env x = some T
   -> TYPE env (XVar x) T

 | TYLam 
   :  forall env x T11 T12 t12
   ,  TYPE (extend env x T11) t12 T12
   -> TYPE env                (XLam x T11 t12) (TFun T11 T12)

 | TYApp 
   :  forall env t1 t2 T11 T12
   ,  TYPE env t1 (TFun T11 T12)
   -> TYPE env t2 T11
   -> TYPE env (XApp t1 t2) T12.

Hint Constructors TYPE.


Example type_example1 
 : TYPE empty (XLam nA tA (XVar nA)) (TFun tA tA).
Proof. auto. Qed.


Example type_example2 
 : TYPE empty 
        (XLam nA tA 
         (XLam nB (TFun tA tA)
          (XApp (XVar nB) (XApp (XVar nB) (XVar nA)))))
        (TFun tA (TFun (TFun tA tA) tA)).
Proof.
 apply TYLam.  apply TYLam.
 eapply TYApp. apply TYVar. apply extend_eq. 
 eapply TYApp. apply TYVar. apply extend_eq. 
 eapply TYVar. unfold extend. simpl. tauto.
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

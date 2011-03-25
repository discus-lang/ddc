
Require Export Base.
Require Export Name.
Require Export Context.
Require Export Ty.
Require Export Exp.

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


(* A well typed expression with a free variable
   has that variable in the type environment.  
 *)
Lemma tyenv_contains_free_vars
 :  forall x t T env
 ,  freeX x t
 -> TYPE env t T
 -> (exists T', env x = some T').
Proof.
 intros.
 generalize dependent env.
 generalize dependent T.
 induction H ; intros.
 Case "XVar".
  inversion H0. subst.
  exists T. assumption.
 Case "XLam".
  inversion H1. subst.
  apply IHfreeX in H7. 
  rewrite extend_neq in H7. assumption.
  assumption.
 Case "XApp/app1". 
  inversion H0. subst.
  eapply IHfreeX. apply H4.
 Case "XApp/app2".
  inversion H0. subst.
  eapply IHfreeX. apply H6.
Qed.


(* We can ignore elements of the type environment if that
   variable is not free in the expresison being checked. 
 *)
Lemma tyenv_invariance 
 : forall env env' t T
 ,  TYPE env t T
 -> (forall x, freeX x t -> env x = env' x)
 -> TYPE env' t T.
Proof.
 intros.
 generalize dependent env'.
 induction H; intros.
 Case "XVar".
  apply TYVar. rewrite <- H0. assumption. apply FreeX_var.
 Case "XLam".
  apply TYLam. apply IHTYPE.
  intros.
  unfold extend. remember (beq_name x x0) as e. destruct e.
   trivial.
   eapply H0. eapply FreeX_lam. apply false_name_neq. assumption.
   assumption.
 Case "XApp".
  eapply TYApp.
   eauto. eauto.
Qed.


(* If well typed term is closed, when we can also check it in
   an empty type environment.
 *)
Theorem check_closed_in_empty
 : forall env t T 
 , closed t -> TYPE env t T -> TYPE empty t T.
Proof.
 intros.
 eapply tyenv_invariance.
 apply H0.
 intros. contradict H1. unfold closed in H. apply H.
Qed.


(* If we can check a term in an empty environment, then it is closed.
   This is helpful when substituting values into terms, because the 
   substitution of closed values cannot cause variable capture probles.
 *)
Theorem check_empty_is_closed
 : forall t T
 , TYPE empty t T -> closed t.
Proof.
 intros t T.
 generalize T.
 induction t.
 Case "XVar".
  intros. inversion H. subst. unfold empty in H2. inversion H2.
 Case "XLam".
  intros. inversion H. subst.
  unfold closed. intro.
  unfold not. intro.
  eapply tyenv_contains_free_vars with (x:=x) in H.
  inversion H. inversion H1. assumption.
 Case "XApp".
  intros. inversion H. subst.
  apply closed_app.
  eapply IHt1. apply H3.
  eapply IHt2. apply H5.
Qed.


(* Substitution of values in values preserves typing.
 *)
Lemma subst_value_value
 :  forall env x val t1 T1 T2
 ,  (forall z, freeX z val -> freshX z t1)
 -> TYPE (extend env x T2) t1  T1
 -> TYPE env               val T2
 -> TYPE env  (subst x val t1) T1.
Proof.
 intros env x val t1 T1 T2.
 generalize dependent env.
 generalize dependent T1.
 induction t1.
 Case "XVar".
  intros. simpl. rename n into y.
  remember (beq_name x y) as e. destruct e.
  SCase "x=y".
   apply true_name_eq in Heqe. subst.
   inversion H0. subst.
   rewrite extend_eq in H4. inversion H4. subst. assumption.
  SCase "x<>y".
   apply TYVar.
   inversion H0. subst.
   apply false_name_neq in Heqe.
   rewrite extend_neq in H4. assumption. assumption.
 Case "XLam".
  intros. rename n into y.
  simpl. remember (beq_name x y) as e. destruct e.
  SCase "x=y".
   apply true_name_eq in Heqe. subst.
   eapply tyenv_invariance.
    apply H0.
    intros. apply extend_neq.
     apply nocapture_lam in H2. apply sym_not_equal in H2. assumption.
  SCase "x<>y".
   apply false_name_neq in Heqe.
   inversion H0. subst.
   apply TYLam. apply IHt1. 
    intros. apply H in H2. inversion H2. subst. assumption.
    rewrite extend_swap.
     assumption.
     apply sym_not_equal. assumption.
    eapply tyenv_invariance.
     apply H1.
     intros. apply H in H2. inversion H2. subst.
      rewrite -> extend_neq. trivial. apply sym_not_equal. assumption.
 Case "XApp".
   intros. simpl. inversion H0. subst.
   eapply TYApp.
   eapply IHt1_1 in H5. apply H5. 
    intros. apply H in H2. inversion H2. subst. assumption. assumption.
   eapply IHt1_2 in H7. apply H7.
    intros. apply H in H2. inversion H2. subst. assumption. assumption.
Qed.


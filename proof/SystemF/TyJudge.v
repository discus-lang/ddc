
Require Export Base.
Require Export Name.
Require Export Context.
Require Export Ty.
Require Export Exp.


(* Type environment contains types of free value vars *)
Definition tyenv := partial_map ty.


(* Kind environment contains kinds of free type vars *)
Definition kienv := partial_map ki.

(* Check well formdness of types. 
   Regular System-F isn't higher kinded, so we just need to check
   that all the type variables are in the correct namespace.
 *)


(* Check well typeness of terms. *)
Inductive TYPE : kienv -> tyenv -> exp -> ty -> Prop :=
 | TYVar 
   :  forall kenv tenv x T
   ,  space_of_name x = SpaceValue
   -> tenv x = some T
   -> TYPE kenv tenv (XVar x) T

 | TYLam 
   :  forall kenv tenv x T11 T12 t12
   ,  space_of_name x = SpaceValue
   -> TYPE kenv (extend tenv x T11) t12 T12
   -> TYPE kenv tenv                (XLam x T11 t12) (TFun T11 T12)

 | TYApp 
   :  forall kenv tenv t1 t2 T11 T12
   ,  TYPE kenv tenv t1 (TFun T11 T12)
   -> TYPE kenv tenv t2 T11
   -> TYPE kenv tenv (XApp t1 t2) T12

 | TYLAM
   :  forall kenv tenv a t12 T12
   ,  space_of_name a = SpaceType
   -> TYPE (extend kenv a KStar) tenv t12 T12 
   -> TYPE kenv tenv (XLAM a t12) (TForall a T12)

 | TYAPP
   :  forall kenv tenv a T11 t1 T2
   ,  TYPE kenv tenv t1 (TForall a T11)
   -> TYPE kenv tenv (XAPP t1 T2) (substTT a T2 T11). 

Hint Constructors TYPE.


(* If a well typed expression has a free value variable, 
   then that variable appears in the type environment.
 *)
Lemma tyenv_contains_free_valvars
 :  forall x t T kenv tenv
 ,  freeX x t
 -> space_of_name x = SpaceValue
 -> TYPE kenv tenv t T
 -> (exists T', tenv x = some T').
Proof.
 intros.
 generalize dependent kenv.
 generalize dependent tenv.
 generalize dependent T.
 induction H; intros.
 Case "XVar".
  inversion H1. subst. exists T. auto.
 Case "XLam".
  inversion H2. subst.
  apply IHfreeX in H10.  
  rewrite extend_neq in H10; auto. auto.
 Case "XApp/app1". 
  inversion H1. subst.
  eapply IHfreeX. eauto. eauto.
 Case "XApp/app2".
  inversion H1. subst.
  eapply IHfreeX. eauto. eauto.
 Case "XLAM".
  inversion H2. subst.
  apply IHfreeX in H9. auto. auto.
 Case "XAPP/APP1".
  inversion H1. subst.
  eapply IHfreeX. auto. eauto.
 Case "XApp/APP2".
  inversion H1. subst.
  inversion H6. subst.
  
  inversion H. subst.
  
  ea

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
  apply TYVar. rewrite <- H0; auto.
 Case "XLam".
  apply TYLam. apply IHTYPE.
  intros. unfold extend.
  remember (beq_name x x0) as e. destruct e; auto.
 Case "XApp".
  eapply TYApp; eauto.
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
  eauto. 
  intros. contradict H1. eauto.
Qed.


(* If we can check a term in an empty environment, then it is closed.
   This is helpful when substituting values into terms, because the 
   substitution of closed values cannot cause variable capture probles.
 *)
Theorem check_empty_is_closed
 : forall t T
 , TYPE empty t T -> closed t.
Proof.
 intros.
 unfold closed. intro. unfold not. intro.
 eapply tyenv_contains_free_vars in H.
  destruct H. unfold empty in H. inversion H.
  eauto.
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
   rewrite extend_eq in H4. inversion H4. subst. eauto.
  SCase "x<>y".
   apply false_name_neq in Heqe.
   apply TYVar.
   inversion H0. subst.
   rewrite extend_neq in H4; auto.

 Case "XLam".
  intros. rename n into y.
  simpl. remember (beq_name x y) as e. destruct e.
  SCase "x=y".
   apply true_name_eq in Heqe. subst.
   eapply tyenv_invariance.
    eauto.
    intros. apply extend_neq. apply nocapture_lam in H2. auto. 
  SCase "x<>y".
   apply false_name_neq in Heqe.
   inversion H0. subst.
   apply TYLam. apply IHt1. 
    intros. apply H in H2. inversion H2. subst. auto.
    rewrite extend_swap; auto.
    eapply tyenv_invariance.
     eauto.
     intros. apply H in H2. inversion H2. subst. eauto.
      rewrite -> extend_neq; auto. 

 Case "XApp".
   intros. simpl. inversion H0. subst.
   eapply TYApp.
   eapply IHt1_1 in H5. eauto. 
    intros. apply H in H2. inversion H2. subst. auto. auto.
   eapply IHt1_2 in H7. eauto.
    intros. apply H in H2. inversion H2. subst. auto. auto.
Qed.


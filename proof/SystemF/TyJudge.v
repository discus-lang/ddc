
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
Inductive KIND : kienv -> ty -> ki -> Prop :=
 | KICon 
   :  forall kenv x
   ,  KIND kenv (TCon x) KStar

 | KIVar
   :  forall kenv x K
   ,  space_of_name x = SType
   -> kenv x          = some K
   -> KIND kenv (TVar x) KStar

 | KIForall
   :  forall kenv a T
   ,  space_of_name a = SType
   -> KIND (extend kenv a KStar) T KStar
   -> KIND kenv (TForall a T) KStar

 | KIFun 
   :  forall kenv t1 t2
   ,  KIND kenv t1 KStar
   -> KIND kenv t2 KStar
   -> KIND kenv (TFun t1 t2) KStar.

Hint Constructors KIND.


(* For well formed types, the only free variables are type variables *)
Lemma only_type_vars_in_types
 :  forall a kenv T K
 ,  freeT a T
 -> KIND kenv T K
 -> space_of_name a = SType.
Proof. 
 intros.
 generalize dependent kenv. 
 induction T.
 Case "TCon".
  inversion H.
 Case "TVar".
  intros.
  inversion H0. subst.
  inversion H.  subst. auto.
 Case "TForall".
  intros.
  eapply IHT. 
  inversion H.  subst. auto.
  inversion H0. subst. eauto.
 Case "TFun".
  intros. inversion H0. subst.
  inversion H. subst.
  eapply IHT1; eauto.
  eapply IHT2; eauto.
Qed.


(* Check well typeness of terms. *)
Inductive TYPE : kienv -> tyenv -> exp -> ty -> Prop :=
 | TYVar 
   :  forall kenv tenv x T
   ,  space_of_name x = SValue
   -> tenv x = some T
   -> TYPE kenv tenv (XVar x) T

 | TYLam 
   :  forall kenv tenv x T11 T12 t12
   ,  space_of_name x = SValue
   -> TYPE kenv (extend tenv x T11) t12 T12
   -> TYPE kenv tenv (XLam x T11 t12) (TFun T11 T12)

 | TYApp 
   :  forall kenv tenv t1 t2 T11 T12
   ,  TYPE kenv tenv t1 (TFun T11 T12)
   -> TYPE kenv tenv t2 T11
   -> TYPE kenv tenv (XApp t1 t2) T12

 | TYLAM
   :  forall kenv tenv a t12 T12
   ,  space_of_name a = SType
   -> TYPE (extend kenv a KStar) tenv t12 T12 
   -> TYPE kenv tenv (XLAM a t12) (TForall a T12)

 | TYAPP
   :  forall kenv tenv a T11 t1 T2
   ,  TYPE kenv tenv t1 (TForall a T11)
   -> KIND kenv T2 KStar
   -> TYPE kenv tenv (XAPP t1 T2) (substTT a T2 T11). 

Hint Constructors TYPE.


(* If a well typed expression has a free value variable, 
   then that variable appears in the type environment.
 *)
Lemma tyenv_contains_free_valvars
 :  forall x t T kenv tenv
 ,  freeX x t
 -> space_of_name x = SValue
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
  eapply only_type_vars_in_types with (a:=x) in H8.
  rewrite H0 in H8. inversion H8.
  auto.
Qed.

Definition freeX_type a t
 := space_of_name a = SType -> freeX a t.
Hint Unfold freeX_type.

Definition freeX_value x t
 := space_of_name x = SValue -> freeX x t.
Hint Unfold freeX_value.


(* We can check a type with a larger kind environment, 
   and get the same kind. *)
Lemma kienv_invariance
 :  forall kenv kenv' T K
 ,  KIND kenv  T K
 -> (forall a, freeT a T -> kenv a = kenv' a)
 -> KIND kenv' T K.
Proof.
 intros.
 generalize dependent kenv'.
 induction H; intros.
 Case "TCon".
  auto.
 Case "TVar".
  eapply KIVar. auto. rewrite <- H1.
  eauto. auto.
 Case "TForall".
  apply KIForall. auto.
  apply IHKIND.
   intros.
   unfold extend. remember (beq_name a a0) as e. destruct e.
   auto. apply false_name_neq in Heqe. eauto.
 Case "TFun".
  apply KIFun; auto.
Qed.


(* We can check an exp with larger kind and type environments,
   and get the same type.
 *)
Lemma tyenv_invariance 
 :  forall tenv tenv' kenv kenv' t T
 ,  TYPE kenv tenv t T
 -> (forall a, freeX_type  a t -> kenv a = kenv' a)
 -> (forall x, freeX_value x t -> tenv x = tenv' x)
 -> TYPE kenv' tenv' t T.
Proof.
 intros.
 generalize dependent tenv'.
 generalize dependent kenv'.
 induction H; intros.
 Case "XVar".
  apply TYVar. auto. rewrite <- H0.
  symmetry. eapply H2. auto.
 Case "XLam".
  apply TYLam. auto.
  apply IHTYPE.
   intros. apply H1.
    unfold freeX_type. intro.
    apply FreeX_lam. unfold not. intro. subst.
    rewrite H in H4. inversion H4. auto.
   intros. unfold extend.
    remember (beq_name x x0) as e. destruct e; auto.
    apply H2. unfold freeX_value. intro.
    apply FreeX_lam. unfold not. intro. subst.
    apply false_name_neq in Heqe. tauto.
    auto.
 Case "XApp".
  eapply TYApp.
   apply IHTYPE1; auto.
   apply IHTYPE2; auto.
 Case "XLAM".
  eapply TYLAM.
   auto. apply IHTYPE.
    intros. unfold extend.
     remember (beq_name a a0) as e. destruct e; auto.
     apply H1. unfold freeX_type. intro.
     apply FreeX_LAM. unfold not. intro. subst.
     apply false_name_neq in Heqe. tauto.
     auto.
    intros. apply H2. unfold freeX_value. intro.
     apply FreeX_LAM. unfold not. intro. subst.
     rewrite H in H4. inversion H4. auto.
 Case "XAPP".
  apply TYAPP.
   apply IHTYPE.
    intros. apply H1. eauto.
    intros. apply H2. eauto.
   eapply kienv_invariance.
    eauto.
    intros. apply H1. eauto.
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


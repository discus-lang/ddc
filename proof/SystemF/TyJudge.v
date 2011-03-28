
Require Export Base.
Require Export Name.
Require Export Context.
Require Export Exp.
Require Export Ty.
Require Export KiJudge.

(* Type environment contains types of free value vars *)
Definition tyenv := partial_map ty.


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


(* If a well formed expresion has a free kind variable, 
   then that variable appears in the kind environment.
 *)
Lemma type_kienv_contains_free_tyvars
 :  forall a t T kenv tenv
 ,  TYPE kenv tenv t T
 -> space_of_name a = SType
 -> freeX a t 
 -> (exists K', kenv a = some K').
Proof. 
 intros.
 generalize dependent kenv.
 generalize dependent tenv.
 generalize dependent T.
 induction t.
 Case "XVar".
  intros. inversion H1. subst.
  inversion H. subst. rewrite H0 in H3. inversion H3.
 Case "XLam".
  intros. inversion H1. subst.
  inversion H. subst.
  eapply IHt. auto. eauto.
 Case "XApp".
  intros. inversion H. subst.
  inversion H1.
   subst. apply IHt1 in H6. auto. auto.
   subst. apply IHt2 in H8. auto. auto.
 Case "XLAM".
  intros. inversion H1. subst.
  inversion H. subst.
  apply IHt in H10. destruct H10. exists x.
  rewrite extend_neq in H2. auto. auto. auto.
 Case "XAPP".
  intros.  inversion H1.
  subst. inversion H. subst. eapply IHt; eauto.
  subst. inversion H. subst.
  eapply kind_kienv_contains_free_tyvars; eauto.
Qed.


(* If a well typed expression has a free value variable, 
   then that variable appears in the type environment.
 *)
Lemma type_tyenv_contains_free_valvars
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


(* We can check an exp with larger kind and type environments,
   and get the same type.
 *)
Lemma type_tyenv_invariance 
 :  forall tenv tenv' kenv kenv' t T
 ,  TYPE kenv tenv t T
 -> (forall a, space_of_name a = SType  
            -> freeX a t -> kenv a = kenv' a)
 -> (forall x, space_of_name x = SValue
            -> freeX x t -> tenv x = tenv' x)
 -> TYPE kenv' tenv' t T.
Proof.
 intros.
 generalize dependent tenv'.
 generalize dependent kenv'.
 induction H; intros.
 Case "XVar".
  apply TYVar. auto. rewrite <- H0.
  symmetry. eapply H2; auto.
 Case "XLam".
  apply TYLam. auto.
  apply IHTYPE.
   intros. apply H1. auto.
    apply FreeX_lam. unfold not. intro. subst.
    rewrite H in H3. inversion H3. auto.
   intros. unfold extend.
    remember (beq_name x x0) as e. destruct e; auto.
    apply H2. auto.
    apply FreeX_lam. unfold not. intro. subst.
    apply false_name_neq in Heqe. tauto. auto.
 Case "XApp".
  eapply TYApp; auto.
 Case "XLAM".
  eapply TYLAM.
   auto. apply IHTYPE.
    intros. unfold extend.
     remember (beq_name a a0) as e. destruct e; auto.
     apply H1. auto.
     apply FreeX_LAM. unfold not. intro. subst.
      apply false_name_neq in Heqe. tauto. auto.
    intros. apply H2. auto.
     apply FreeX_LAM. unfold not. intro. subst.
     rewrite H in H3. inversion H3. auto.
 Case "XAPP".
  apply TYAPP. auto.
  eapply kind_kienv_invariance.
   eauto.
   intros. apply H1. auto. auto.
Qed.


(* If well typed term is closed, when we can also check it in
   an empty type environment.
 *)
Theorem check_closed_in_empty
 : forall kenv tenv t T 
 , closed t -> TYPE kenv tenv t T -> TYPE empty empty t T.
Proof.
 intros.
 eapply type_tyenv_invariance.
  eauto.
  intros. unfold closed in H. specialize H with a. tauto.
  intros. unfold closed in H. specialize H with x. tauto.
Qed.


(* If we can check a term in an empty environment, then it is closed.
   This is helpful when substituting values into terms, because the 
   substitution of closed values cannot cause variable capture probles.
 *)
Theorem check_empty_is_closed
 : forall t T
 , TYPE empty empty t T -> closed t.
Proof.
 intros.
 unfold closed. intro. unfold not.
 intro.
 destruct x. 
 remember (Name s n) as v.
 destruct s.
  eapply type_tyenv_contains_free_valvars with (tenv:=empty) in H0.
   destruct H0. unfold empty in H0. inversion H0.
   subst. simpl. auto. eauto.
  eapply type_kienv_contains_free_tyvars with (kenv:=empty) (a:=v) in H.
   destruct H.  unfold empty in H. inversion H.
   subst. auto. auto.
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
   eapply type_tyenv_invariance.
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


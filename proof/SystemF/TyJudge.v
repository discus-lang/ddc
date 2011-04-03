
Require Export Base.
Require Export Name.
Require Export Context.
Require Export Exp.
Require Export Ty.
Require Export KiJudge.


(* Check well typeness of terms. *)
Inductive TYPE : kienv -> tyenv -> exp -> ty -> Prop :=
 | TYVar 
   :  forall kenv tenv x T
   ,  valname x 
   -> tenv x = Some T
   -> TYPE kenv tenv (XVar x) T

 | TYLam 
   :  forall kenv tenv x T11 T12 t12
   ,  valname x
   -> TYPE kenv (extend tenv x T11) t12 T12
   -> TYPE kenv tenv (XLam x T11 t12) (TFun T11 T12)

 | TYApp 
   :  forall kenv tenv t1 t2 T11 T12
   ,  TYPE kenv tenv t1 (TFun T11 T12)
   -> TYPE kenv tenv t2 T11
   -> TYPE kenv tenv (XApp t1 t2) T12

 | TYLAM
   :  forall kenv tenv a t12 T12
   ,  tyname a
   -> TYPE (extend kenv a KStar) tenv t12 T12 
   -> TYPE kenv tenv (XLAM a t12) (TForall a T12)

 | TYAPP
   :  forall kenv tenv a T11 t1 T2
   ,  tyname a
   -> TYPE kenv tenv t1 (TForall a T11)
   -> KIND kenv T2 KStar
   -> TYPE kenv tenv (XAPP t1 T2) (substTT a T2 T11). 

Hint Constructors TYPE.


(* If a well formed expresion has a free kind variable, 
   then that variable appears in the kind environment.
 *)
Lemma type_kienv_contains_free_tyvars
 :  forall a t T kenv tenv
 ,  TYPE kenv tenv t T
 -> tyname a -> freeX a t 
 -> (exists K', kenv a = Some K').
Proof. 
 intros. gen kenv tenv T.
 induction t; intros.
 Case "XVar".
  inversions H1.
  inversions H.
   unfold tyname in H0.
   unfold valname in H2. 
   rewrite H0 in H2. inversion H2.
 Case "XLam".
  inversions H1.
  inversions H.
  eapply IHt; eauto.
 Case "XApp".
  inversions H.
  inversions H1.
   apply IHt1 in H6; auto.
   apply IHt2 in H8; auto.
 Case "XLAM".
  inversions H1.
  inversions H.
  apply IHt in H9. destruct H9. exists x.
  rewrite extend_neq in H; auto. auto.
 Case "XAPP".
  inversions H1.
   inversions H. eapply IHt; eauto.
   inversions H. eapply kind_kienv_contains_free_tyvars; eauto.
Qed.


(* If a well typed expression has a free value variable, 
   then that variable appears in the type environment.
 *)
Lemma type_tyenv_contains_free_valvars
 :  forall x t T kenv tenv
 ,  valname x -> freeX x t
 -> TYPE kenv tenv t T
 -> (exists T', tenv x = Some T').
Proof.
 intros. gen kenv tenv T.
 induction H0; intros.
 Case "XVar".
  inversions H1. exists T. auto.
 Case "XLam".
  inversions H2.
  apply IHfreeX in H10; auto.
  rewrite extend_neq in H10; auto.
 Case "XApp/app1". 
  inversions H1.
  eapply IHfreeX; auto. eauto.
 Case "XApp/app2".
  inversions H1.
  eapply IHfreeX; auto. eauto.
 Case "XLAM".
  inversions H2.
  apply IHfreeX in H9; auto.
 Case "XAPP/APP1".
  inversions H1.
  eapply IHfreeX; auto. eauto.
 Case "XApp/APP2".
  inversions H1.
  eapply only_type_vars_in_types with (a:=x) in H9.
  inversion H.
  rewrite H9 in H2. inversion H2.
  auto.
Qed.


(* We can check an exp with larger kind and type environments,
   and get the same type.
 *)
Lemma type_tyenv_invariance 
 :  forall tenv tenv' kenv kenv' t T
 ,  TYPE kenv tenv t T
 -> (forall a, tyname  a -> freeX  a t -> kenv' a = kenv a)
 -> (forall x, valname x -> freeX  x t -> tenv' x = tenv x)
 -> TYPE kenv' tenv' t T.
Proof.
 intros. gen tenv' kenv'.
 induction H; intros.

 Case "XVar".
  apply TYVar.
   auto.
   specialize H1 with x. apply H1 in H.
   rewrite H. auto. auto.

 Case "XLam".
  apply TYLam. auto.
  apply IHTYPE.
   intros. unfold extend. breaka (beq_name x x0).
    eauto. eauto. eauto.

 Case "XApp".
  eapply TYApp; auto.

 Case "XLAM".
  eapply TYLAM. auto. auto.
  apply IHTYPE.
   eauto.
   intros. unfold extend. breaka (beq_name a a0).
    apply false_name_neq in HeqX.
    apply H2. eauto. eauto.

 Case "XAPP".
  apply TYAPP. auto. auto.
  eapply kind_kienv_invariance.
   eauto.
   intros. symmetry. eapply H3. eauto. eauto.
Qed.


(* If well typed term is closed, when we can also check it in
   an empty type environment.
 *)
Theorem check_closedX_in_empty
 : forall kenv tenv t T 
 , closedX t -> TYPE kenv tenv t T -> TYPE empty empty t T.
Proof.
 intros.
 eapply type_tyenv_invariance.
  eauto.
  intros. unfold closedX in H. specialize H with a. tauto.
  intros. unfold closedX in H. specialize H with x. tauto.
Qed.


(* If we can check a term in an empty environment, then it is closed.
   This is helpful when substituting values into terms, because the 
   substitution of closed values cannot cause variable capture probles.
 *)
Theorem check_empty_is_closed
 : forall t T
 , TYPE empty empty t T -> closedX t.
Proof.
 intros.
 unfold closedX. intro. intro.
 destruct x. 
 remember (Name s n) as v.
 destruct s. 
  eapply type_tyenv_contains_free_valvars with (tenv:=empty) in H0.
   destruct H0. unfold empty in H0. inversion H0.
   subst. auto. eauto.
  eapply type_kienv_contains_free_tyvars with (kenv:=empty) (a:=v) in H.
   destruct H.  unfold empty in H.  inversion H.
   subst. auto. auto.
Qed.




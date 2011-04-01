
Require Export Base.
Require Export Name.
Require Export Context.
Require Export Ty.
Require Export Exp.


(* Kind environment contains kinds of free type vars *)
Definition kienv := partial_map ki.


(* Well formdness of types. 
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
   -> kenv x          = Some K
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


(* If a well formed type has a free variable, 
   then it's guaranteed to be a type variable.
 *)
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
  inversions H.
  inversions H0. auto.
 Case "TForall".
  intros.
  eapply IHT. 
  inversions H.  auto. 
  inversions H0. eauto.
 Case "TFun".
  intros.
  inversions H0.
  inversions H.
  eapply IHT1; eauto.
  eapply IHT2; eauto.
Qed.


(* If a well formed type has a free kind variable, 
   then that variable appears in the kind environment. 
 *)
Lemma kind_kienv_contains_free_tyvars
 :  forall kenv T K a
 ,  KIND kenv T K
 -> tyname a -> freeT a T
 -> (exists K', kenv a = Some K').
Proof. 
 intros.
 generalize dependent kenv.
 generalize dependent K.
 induction T; intros.
 Case "TCon".
  inversion H1.
 Case "TVar".
  inversions H1.
  inversions H. eauto.
 Case "TForall".
  inversions H1.
  inversions H.
  apply IHT in H8. destruct H8. exists x.
  rewrite extend_neq in H; auto. auto.
 Case "TFun".
  inversions H.
  inversions H1; eauto.
Qed.


(* We can check a type with a larger kind environment, 
   and get the same kind. 
 *)
Lemma kind_kienv_invariance
 :  forall kenv kenv' T K
 ,  KIND kenv  T K
 -> (forall a, tyname a -> freeT a T -> kenv a = kenv' a)
 -> KIND kenv' T K.
Proof.
 intros.
 generalize dependent kenv'.
 induction H; intros.
 Case "TCon".
  auto.
 Case "TVar".
  eapply KIVar; auto.
  rewrite <- H1; eauto.
 Case "TForall".
  apply KIForall. auto.
  apply IHKIND.
   intros.
   unfold extend. breaka (beq_name a a0).
   apply false_name_neq in HeqX. eauto.
 Case "TFun".
  apply KIFun; auto.
Qed.


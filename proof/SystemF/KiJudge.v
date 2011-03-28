
Require Export Base.
Require Export Name.
Require Export Context.
Require Export Ty.
Require Export Exp.


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


(* If a well formed type has a free kind variable, 
   then that variable appears in the kind environment. *)
Lemma kind_kienv_contains_free_tyvars
 :  forall kenv T K a
 ,  KIND kenv T K
 -> space_of_name a = SType
 -> freeT a T
 -> (exists K', kenv a = some K').
Proof. 
 intros.
 generalize dependent kenv.
 generalize dependent K.
 induction T.
 Case "TCon".
  intros. inversion H1.
 Case "TVar".
  intros. inversion H1. subst.
  inversion H. subst. eauto.
 Case "TForall".
  intros. inversion H1. subst.
  inversion H. subst.
  apply IHT in H9. destruct H9. exists x.
  rewrite extend_neq in H2; auto. auto.
 Case "TFun".
  intros. inversion H. subst.
  inversion H1; subst; eauto.
Qed.


(* We can check a type with a larger kind environment, 
   and get the same kind. *)
Lemma kienv_invariance
 :  forall kenv kenv' T K
 ,  KIND kenv  T K
 -> (forall a, space_of_name a = SType
            -> freeT a T -> kenv a = kenv' a)
 -> KIND kenv' T K.
Proof.
 intros.
 generalize dependent kenv'.
 induction H; intros.
 Case "TCon".
  auto.
 Case "TVar".
  eapply KIVar. auto. rewrite <- H1.
  eauto. auto. auto.
 Case "TForall".
  apply KIForall. auto.
  apply IHKIND.
   intros.
   unfold extend. remember (beq_name a a0) as e. destruct e.
   auto. apply false_name_neq in Heqe. eauto.
 Case "TFun".
  apply KIFun; auto.
Qed.




Require Export DDC.Language.SystemF2.Ty.
Require Export DDC.Language.SystemF2.Ki.


(* Kinds judgement assigns a kind to a type *)
Inductive KIND : kienv -> ty -> ki -> Prop :=
 | KIConFun
   :  forall ke
   ,  KIND ke (TCon TyConFun) (KFun KStar (KFun KStar KStar))

 | KIConData
   :  forall ke i k
   ,  KIND ke (TCon (TyConData i k)) k

 | KIVar
   :  forall ke i k
   ,  get i ke = Some k
   -> KIND ke (TVar i) k

 | KIForall
   :  forall ke t
   ,  KIND (ke :> KStar) t           KStar
   -> KIND ke            (TForall t) KStar

 | KIApp 
   :  forall ke t1 t2 k11 k12
   ,  KIND ke t1 (KFun k11 k12)
   -> KIND ke t2 k11
   -> KIND ke (TApp t1 t2) k12.
Hint Constructors KIND.


(* A well kinded type is well formed *)
Lemma kind_wfT
 :  forall ke t k
 ,  KIND ke t k
 -> wfT  ke t.
Proof.
 intros. gen ke k.
 induction t; intros; inverts H; simpl; eauto.
Qed.


(* If a type is well kinded in an empty environment,
   then that type is closed. *)
Lemma kind_empty_is_closed
 :  forall t k
 ,  KIND nil t k 
 -> closedT t.
Proof.
 intros. unfold closedT. eapply kind_wfT. eauto.
Qed.


(* Weakening kind environments. *)
Lemma kind_kienv_insert
 :  forall ke ix t k1 k2
 ,  KIND ke t k1
 -> KIND (insert ix k2 ke) (liftTT ix t) k1.
Proof.
 intros. gen ix ke k1.
 induction t; intros; simpl; inverts H; eauto.

 Case "TVar".
  lift_cases; intros; auto.

 Case "TForall".
  apply KIForall.
  rewrite insert_rewind.
  apply IHt. auto.
Qed.


Lemma kind_kienv_weaken
 :  forall ke t k1 k2
 ,  KIND  ke         t           k1
 -> KIND (ke :> k2) (liftTT 0 t) k1.
Proof.
 intros.
 assert (ke :> k2 = insert 0 k2 ke). simpl.
   destruct ke; auto.
 rewrite H0. apply kind_kienv_insert. auto.
Qed.




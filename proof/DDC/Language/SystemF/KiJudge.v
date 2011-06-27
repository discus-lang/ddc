
Require Export DDC.Language.SystemF.Ty.
Require Export DDC.Language.SystemF.Ki.


(* Kind judgement assigns a kind to a type.

   With plain SystemF (not SystemF2) we only have value types.
   We could have made this a 2-place relation and omitted the 
   KStar return kind, but doing it this way makes the form of the 
   judgement more similar to the SystemF2 case. 
 *)
Inductive KIND : kienv -> ty -> ki -> Prop :=
 | KICon 
   :  forall ke c
   ,  KIND ke (TCon c) KStar

 | KIVar
   :  forall ke i k
   ,  get  i ke = Some k
   -> KIND ke (TVar i) k

 | KIForall
   :  forall ke t
   ,  KIND (ke :> KStar) t           KStar
   -> KIND ke            (TForall t) KStar

 | KIFun 
   :  forall ke t1 t2
   ,  KIND ke t1 KStar
   -> KIND ke t2 KStar
   -> KIND ke (TFun t1 t2) KStar.

Hint Constructors KIND.


(********************************************************************)
(* A well kinded type is well formed. *)
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


(* We can insert a new type into the environment at an arbitray point
   provided we lift existing references to types higher than this
   point across the new one. *)
Lemma liftTT_insert
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


(* Weakening the kind environment by pushing a new type onto the
   stack *)
Lemma liftTT_weaken
 :  forall ke t k1 k2
 ,  KIND  ke         t           k1
 -> KIND (ke :> k2) (liftTT 0 t) k1.
Proof.
 intros.
 assert (ke :> k2 = insert 0 k2 ke). simpl.
   destruct ke; auto.
 rewrite H0. apply liftTT_insert. auto.
Qed.


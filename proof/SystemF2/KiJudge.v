
Require Export Ty.
Require Export Ki.
Require Export Env.


(* Kinds of types ***************************************************)
Inductive KIND : kienv -> ty -> ki -> Prop :=
 | KIConFun
   :  forall ke
   ,  KIND ke (TCon TyConFun) (KFun KStar (KFun KStar KStar))

 | KIConData
   :  forall ke i k
   ,  KIND ke (TCon (TyConData i k)) k

 | KIVar
   :  forall ke i k
   ,  get  ke i = Some k
   -> KIND ke (TVar i) k

 | KIForall
   :  forall ke t
   ,  KIND (ke :> KStar) t           KStar
   -> KIND ke            (TForall t) KStar

 | KIApp 
   :  forall ke t1 t2
   ,  KIND ke t1 (KFun KStar KStar)
   -> KIND ke t2 KStar
   -> KIND ke (TApp t1 t2) KStar.
Hint Constructors KIND.


(* Well Formedness **************************************************)
(* A well kinded type is well formed *)
Lemma kind_wfT
 :  forall ke t k
 ,  KIND ke t k
 -> wfT  ke t.
Proof.
 intros. gen ke k.
 induction t; intros; inverts H; simpl; eauto.
Qed.


(* Checking closed types ********************************************)
(* If a type is well kinded in an empty environment,
   then that type is closed. *)
Lemma kind_empty_is_closed
 :  forall t k
 ,  KIND Empty t k 
 -> closedT t.
Proof.
 intros. unfold closedT. eapply kind_wfT. eauto.
Qed.



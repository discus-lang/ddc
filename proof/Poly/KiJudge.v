
Require Import Exp.
Require Import WellFormed.
Require Import Base.

(* Kinds of types ***************************************************)

Inductive KIND : kienv -> ty -> ki -> Prop :=
 | KICon 
   :  forall ke c
   ,  KIND ke (TCon c) KStar

 | KIVar
   :  forall ke i k
   ,  get  ke i = Some k
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




Require Export DDC.Language.DiscipleKernel.TyExp.


(********************************************************************)
(* Well formed types are closed under the given kind environment. *)
Inductive wfT (kn: nat) : ty -> Prop :=
 | WfT_TVar 
   :  forall ki
   ,  ki < kn
   -> wfT kn (TVar ki)

 | WfT_TCon
   :  forall n
   ,  wfT kn (TCon n)

 | WfT_TForall
   :  forall k t
   ,  wfT (S kn) t
   -> wfT kn (TForall k t)

 | WfT_TApp
   :  forall t1 t2
   ,  wfT kn t1 -> wfT kn t2
   -> wfT kn (TApp t1 t2)

 | WfT_TSum
   :  forall t1 t2 
   ,  wfT kn t1 -> wfT kn t2
   -> wfT kn (TSum t1 t2)

 | WfT_TBot
   :  forall k
   ,  wfT kn (TBot k).
Hint Constructors wfT.


(* Closed types are well formed under an empty environment. *)
Definition closedT : ty -> Prop
 := wfT O.
Hint Unfold closedT.


(********************************************************************)
(* If a type is well formed under an environment of a given length,
   then it also well formed under a larger one. *)
Lemma wfT_succ
 :  forall tn t1
 ,  wfT tn     t1
 -> wfT (S tn) t1.
Proof.
 intros. gen tn.
 induction t1; intros; inverts H; eauto.
Qed.
Hint Resolve wfT_succ.


Lemma wfT_more
 :  forall tn1 tn2 tt
 ,  tn1 <= tn2
 -> wfT tn1 tt
 -> wfT tn2 tt.
Proof.
 intros. gen tn1 tn2.
 induction tt; intros; inverts H0; eauto.

 Case "TVar".
  eapply WfT_TVar; burn.

 Case "TForall".
  eapply WfT_TForall.
  lets D: IHtt H2 (S tn2).
  eapply D. omega.
Qed.
Hint Resolve wfT_more.


Lemma wfT_max
 :  forall tn1 tn2 tt
 ,  wfT tn1 tt
 -> wfT (max tn1 tn2) tt.
Proof.
 intros.
 assert (  ((tn1 <  tn2) /\ max tn1 tn2 = tn2) 
        \/ ((tn2 <= tn1) /\ max tn1 tn2 = tn1)).
  eapply Max.max_spec.

 inverts H0. rip. rs.
  eapply wfT_more; eauto. 

 inverts H1. rip. rs.
Qed.
Hint Resolve wfT_max.


(********************************************************************)
(* There is an environment that makes a type well formed. *)
Lemma wfT_exists
 :  forall t1
 ,  (exists tn, wfT tn t1).
Proof.
 intros.
 induction t1.
 Case "TVar".
  exists (S n). eauto.

 Case "TCon".
  exists 0. auto.

 Case "TForall".
  shift tn.
  eapply WfT_TForall; eauto.

 Case "TApp".
  destruct IHt1_1 as [tn1].
  destruct IHt1_2 as [tn2].
  exists (max tn1 tn2).
  eapply WfT_TApp. 
   eauto.
   rewrite Max.max_comm. eauto.

 Case "TSum".
  destruct IHt1_1 as [tn1].
  destruct IHt1_2 as [tn2].
  exists (max tn1 tn2).
  eapply WfT_TSum. 
   eauto.
   rewrite Max.max_comm. eauto.

 Case "TBot".
  exists 0. auto.
Qed.
Hint Resolve wfT_exists.


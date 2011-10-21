
Require Import DDC.Base.
Require Import Coq.Bool.Bool.


(********************************************************************)
(* Data Constructors
   Carries a data constructor tag and an arity. *)
Inductive datacon : Type :=
 | DataCon  : nat -> nat -> datacon.
Hint Constructors datacon.


Fixpoint datacon_beq t1 t2 :=
  match t1, t2 with
  | DataCon n11 n12, DataCon n21 n22 
  => beq_nat n11 n21 && beq_nat n12 n22
  end.

Lemma beq_true_and_split
 :  forall a1 a2
 ,  true = a1 && a2
 -> true = a1 /\ true = a2.
Proof.
 destruct a1; destruct a2; auto.
Qed.


Lemma beq_false_and_split
 :  forall a1 a2
 ,  false = a1 && a2
 -> false = a1 \/ false = a2.
Proof.
 destruct a1; destruct a2; auto. 
Qed.


(* Boolean equality for data constructors. *)
Lemma datacon_beq_eq
 :  forall dc dc' 
 ,  true = datacon_beq dc dc'
 -> dc = dc'.
Proof.
 intros.
 destruct dc.
 destruct dc'.
 simpl in H.
  apply beq_true_and_split in H. inverts H.
  apply beq_nat_eq in H0.
  apply beq_nat_eq in H1.
  burn.
Qed.


(* Boolean negation for data constructors. *)
Lemma datacon_beq_false
 :  forall dc 
 ,  false = datacon_beq dc dc 
 -> False.
Proof.
 intro.
 destruct dc.
 simpl.
 intros.
  apply beq_false_and_split in H.
  inverts H.
   induction n. false. auto.
   induction n0. false. auto.
Qed.


Lemma datacon_eq_ex
 : forall (dc1 dc2 : datacon)
 , dc1 = dc2 \/ dc1 <> dc2.
Proof.
 intros.
 assert (exists b, b = datacon_beq dc1 dc2).
  eauto. dest b.
 destruct b. 
  apply datacon_beq_eq in H. subst. auto.
  right. unfold not. intros.
  subst. apply datacon_beq_false in H. auto.
Qed.
Hint Resolve datacon_eq_ex.



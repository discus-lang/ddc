(* Names / Binders *)

Require Export Base.

(* Names. We just use a nat for the unique id *)
Inductive name : Type :=
 Name : nat -> name.


(* Example names *)
Notation nA := (Name 0).
Notation nB := (Name 1).
Notation nC := (Name 2).


(* Names are equal when their ids are equal. *)
Definition beq_name n1 n2 := 
 match (n1, n2) with 
  (Name x1, Name x2) => beq_nat x1 x2
 end.

Hint Unfold beq_name.


(* Reflexivity of name equality *)
Theorem beq_name_refl 
 : forall n
 , true = beq_name n n.
Proof.
 intros. destruct n. unfold beq_name. apply beq_nat_refl.
Qed.


(* Equality of names *************************************)
Theorem beq_name_sym
 : forall n1 n2
 , beq_name n1 n2 = beq_name n2 n1.
Proof.
 intros.
 destruct n1. destruct n2.
 unfold beq_name.
 apply beq_nat_sym.
Qed.


Theorem true_name_eq 
 : forall n1 n2
 , true = beq_name n1 n2 -> n1 = n2.
Proof.
 intros n1 n2. 
 destruct n1.  destruct n2.
 unfold beq_name. 
 intro. apply true_nat_eq in H. subst. trivial.
Qed.


Theorem eq_name_true
 : forall n1 n2
 , n1 = n2 -> true = beq_name n1 n2.
Proof.
 intros n1 n2. intro.
 destruct n1. destruct n2.
 inversion H. subst. 
 apply beq_name_refl.
Qed.


Theorem false_name_neq
 : forall n1 n2
 , false = beq_name n1 n2 -> n1 <> n2.
Proof. 
 intros n1 n2. intro.
 unfold not. intro. subst. contradict H. 
 rewrite <- beq_name_refl. unfold not. intro. inversion H.
Qed.


Theorem neq_name_false
 : forall n1 n2
 , n1 <> n2 -> false = beq_name n1 n2.
Proof. 
 intros n1 n2. intro.
 destruct n1. destruct n2.
 unfold beq_name. apply neq_nat_false. contradict H. subst. trivial.
Qed.


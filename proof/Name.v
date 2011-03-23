
Require Export Base.

Inductive name : Type :=
 Name : nat -> name.


Notation nA := (Name 0).
Notation nB := (Name 1).
Notation nC := (Name 2).


Definition beq_name n1 n2 := 
 match (n1, n2) with 
  (Name x1, Name x2) => beq_nat x1 x2
 end.


Theorem beq_name_refl 
 : forall n
 , true = beq_name n n.
Proof.
 intros. destruct n. apply beq_nat_refl.
Qed.


Theorem beq_name_sym
 : forall n1 n2
 , beq_name n1 n2 = beq_name n2 n1.
Proof.
 intros n1 n2.
 destruct n1. destruct n2.
 unfold beq_name.
 apply beq_nat_sym.
Qed.


(* bool equality ******************************************)
Theorem true_name_eq 
 : forall n1 n2
 , true = beq_name n1 n2 -> n1 = n2.
Proof.
 intros n1 n2. 
 destruct n1. destruct n2.
 unfold beq_name.
 intros eq. apply true_nat_eq in eq. auto.
Qed.


Theorem eq_name_true
 : forall n1 n2
 , n1 = n2 -> true = beq_name n1 n2.
Proof.
 intros n1 n2.
 destruct n1. destruct n2.
 unfold beq_name. intro. inversion H.
 apply eq_nat_true. auto.
Qed.


Theorem false_name_neq
 : forall n1 n2
 , false = beq_name n1 n2 -> n1 <> n2.
Proof. admit. Qed.


Theorem neq_name_false
 : forall n1 n2
 , n1 <> n2 -> false = beq_name n1 n2.
Proof. admit. Qed. 















Require Import Base.

Inductive name : Type :=
 Name : nat -> name.


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


Theorem beq_name_eq 
 : forall n1 n2
 , true = beq_name n1 n2 -> n1 = n2.
Proof.
 intros n1 n2. 
 destruct n1. destruct n2.
 unfold beq_name.
 intros eq. apply beq_nat_eq in eq. auto.
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


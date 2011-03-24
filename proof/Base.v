(* Basic definitions shared by all modules *)

Require Export Cases.


(* bool ***************************************************)
Inductive bool : Type :=
 | true  : bool
 | false : bool.


(* maybe **************************************************)
Inductive option (a : Type) :=
 | none  : option a
 | some  : a -> option a.

Implicit Arguments none [[a]].
Implicit Arguments some [[a]].


(* nat ****************************************************)
Fixpoint beq_nat (n m : nat) : bool :=
 match n, m with
 | O, O       => true
 | O, S _     => false
 | S _, O     => false
 | S n1, S n2 => beq_nat n1 n2
 end.


Theorem beq_nat_refl
 : forall n : nat
 , true = beq_nat n n.
Proof.
 intros. 
 induction n.
  simpl. trivial.
  simpl. assumption. 
Qed.


Theorem beq_nat_sym
 : forall n1 n2 : nat
 , beq_nat n1 n2 = beq_nat n2 n1.
Proof. 
 induction n1.
  intro n2. destruct n2.
   trivial.
   simpl. trivial.
  intro. destruct n2.
   simpl. trivial.
   simpl. apply IHn1.
Qed.


(* Equality of nats ***************************************)
Theorem true_nat_eq
 : forall n1 n2 : nat
 , true = beq_nat n1 n2 -> n1 = n2.
Proof.
 induction n1.
  destruct n2.
   simpl. auto.
   simpl. intro contra. inversion contra.
  intro n2. destruct n2.
   simpl. intro contra. inversion contra.
   simpl. auto.
Qed.


Theorem eq_nat_true
 : forall n1 n2 : nat
 , n1 = n2 -> true = beq_nat n1 n2.
Proof.
 induction n1.
  destruct n2.
   auto.
   simpl. intro. inversion H.
  intro. destruct n2.
   simpl. intro. inversion H.
   simpl. intro. inversion H. apply beq_nat_refl.
Qed.


Theorem false_nat_neq
 : forall n1 n2 : nat
 , false = beq_nat n1 n2 -> n1 <> n2.
Proof.
 induction n1.
  destruct n2.
   simpl. intro. inversion H.
   simpl. intro. unfold not. intro. inversion H0.
  intro n2. destruct n2.
   simpl. intro. unfold not. intro. inversion H0.
   simpl. intro. apply IHn1 in H. contradict H.
    inversion H. subst. trivial. 
Qed.


Theorem neq_nat_false
 : forall n1 n2 : nat
 , n1 <> n2 -> false = beq_nat n1 n2.
Proof.
 induction n1.
  destruct n2.
   intro. contradict H. trivial.
   simpl. intro. trivial.
  intro n2. destruct n2.
   intro. simpl. trivial.
   intro. simpl. apply IHn1. contradict H. subst. trivial.
Qed.



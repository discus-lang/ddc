
Require Export Cases.

(* bool ***************************************************)
Inductive bool : Type :=
 | true  : bool
 | false : bool.


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
  tauto.
  simpl. tauto. 
Qed.


Theorem beq_nat_sym
 : forall n1 n2 : nat
 , beq_nat n1 n2 = beq_nat n2 n1.
Proof. 
 induction n1.
 intro n2. destruct n2.
  tauto. tauto.
  intro. destruct n2. tauto.
  simpl. auto.
Qed.


Theorem true_nat_eq
 : forall n1 n2 : nat
 , true = beq_nat n1 n2 -> n1 = n2.
Proof.
 induction n1.
 destruct n2.
  simpl. tauto.
  simpl. intro contra. inversion contra.
  intros n2. destruct n2.
   simpl. intro contra. inversion contra.
   simpl. intro eq. apply IHn1 in eq. rewrite -> eq. tauto.
Qed.


Theorem eq_nat_true
 : forall n1 n2 : nat
 , n1 = n2 -> true = beq_nat n1 n2.
Proof.
 induction n1.
 destruct n2. tauto.
 simpl. intro. inversion H.
 intro n2. destruct n2.
 simpl. intro C. inversion C.
 intro. inversion H. simpl. apply beq_nat_refl.
Qed.


Theorem false_nat_neq
 : forall n1 n2 : nat
 , false = beq_nat n1 n2 -> n1 <> n2.
Proof.
 induction n1.
 destruct n2.
  simpl. intro. inversion H.
  auto.
  intro n2. destruct n2.
   simpl. intro eq. unfold not. intro. inversion H.
   simpl. intro neq. apply IHn1 in neq. unfold not.
   intro. unfold not in neq. inversion H. rewrite H1 in neq.
   auto.
Qed.


Theorem neq_nat_false
 : forall n1 n2 : nat
 , n1 <> n2 -> false = beq_nat n1 n2.
Proof.
 induction n1.
 destruct n2.
 tauto.
 simpl. tauto.
 intro n2. destruct n2.
 tauto. simpl. auto.
Qed.


(* maybe **************************************************)
Inductive option (a : Type) :=
 | none  : option a
 | some  : a -> option a.

Implicit Arguments none [[a]].
Implicit Arguments some [[a]].
 
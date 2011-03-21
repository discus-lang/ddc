
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


Theorem beq_nat_eq
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


(* maybe **************************************************)
Inductive maybe (a : Type) :=
 | nothing  : maybe a
 | just     : a -> maybe a.

Implicit Arguments nothing [[a]].
Implicit Arguments just    [[a]].
 
(* Names / Binders *)
Require Export Base.

(* Name Spaces *******************************************)
Inductive space : Type :=
 | SValue 
 | SType.


(* Names *************************************************)
Inductive name  : Type :=
 Name : space -> nat -> name.

Definition space_of_name n :=
 match n with
  | Name s _ => s
 end.


(* Names are equal when their ids are equal. *)
Definition beq_name n1 n2 := 
 match (n1, n2) with 
  |  (Name SValue x1, Name SValue x2)
  => beq_nat x1 x2

  |  (Name SType  x1, Name SType  x2)
  => beq_nat x1 x2

  |  (_, _) => false
 end.
Hint Unfold beq_name.


(* Reflexivity of name equality *)
Lemma beq_name_refl 
 : forall n
 , true = beq_name n n.
Proof.
 intros. destruct n. destruct s;
 unfold beq_name; apply beq_nat_refl.
Qed.
Hint Resolve beq_name_refl.


(* Equality of names *************************************)
Lemma beq_name_sym
 : forall n1 n2
 , beq_name n1 n2 = beq_name n2 n1.
Proof.
 intros.
 destruct n1. destruct n2. destruct s. destruct s0.
 unfold beq_name. rewrite beq_nat_sym. auto.
 unfold beq_name. trivial.
 destruct s0.
 unfold beq_name. trivial.
 unfold beq_name. 
 rewrite beq_nat_sym. trivial.
Qed.
Hint Resolve beq_name_sym.


Lemma true_name_eq 
 : forall n1 n2
 , true = beq_name n1 n2 -> n1 = n2.
Proof.
 admit.
Qed.
Hint Resolve true_name_eq.


Lemma eq_name_true
 : forall n1 n2
 , n1 = n2 -> true = beq_name n1 n2.
Proof.
 admit.
Qed.


Lemma false_name_neq
 : forall n1 n2
 , false = beq_name n1 n2 -> n1 <> n2.
Proof. 
 admit.
Qed.
Hint Resolve false_name_neq.


Lemma neq_name_false
 : forall n1 n2
 , n1 <> n2 -> false = beq_name n1 n2.
Proof. 
 admit.
Qed.


(* Example Names ******************************************)
Notation xA := (Name SValue 0).
Notation xB := (Name SValue 1).
Notation xC := (Name SValue 2).


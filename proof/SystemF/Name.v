(* Names / Binders *)

Require Export Base.


(* Space *************************************************)
Inductive space : Type :=
 | SpaceValue 
 | SpaceType.

Definition beq_space s1 s2 :=
 match (s1, s2) with
  | (SpaceValue, SpaceValue) => true
  | (SpaceType,  SpaceType)  => true
  | (_, _)                   => false
 end.

Hint Unfold beq_space.


Theorem beq_space_refl
 : forall s
 , true = beq_space s s.
Proof.
 intros.
 destruct s; eauto.
Qed.


Theorem beq_space_sym
 : forall s1 s2
 , beq_space s1 s2 = beq_space s2 s1.
Proof.
 intros.
 destruct s1. destruct s2; auto. auto.
Qed.


(* Names *************************************************)
Inductive name : Type :=
 Name : space -> nat -> name.

(* Example names *)
Notation xA := (Name SpaceValue 0).
Notation xB := (Name SpaceValue 1).
Notation xC := (Name SpaceValue 2).


(* Names are equal when their ids are equal. *)
Definition beq_name n1 n2 := 
 match (n1, n2) with 
  |  (Name SpaceValue x1, Name SpaceValue x2)
  => beq_nat x1 x2

  |  (Name SpaceType  x1, Name SpaceType  x2)
  => beq_nat x1 x2

  |  (_, _) => false
 end.

Hint Unfold beq_name.


(* Reflexivity of name equality *)
Theorem beq_name_refl 
 : forall n
 , true = beq_name n n.
Proof.
 intros. destruct n. destruct s;
 unfold beq_name; apply beq_nat_refl.
Qed.

Hint Resolve beq_name_refl.


(* Equality of names *************************************)
Theorem beq_name_sym
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


Theorem true_name_eq 
 : forall n1 n2
 , true = beq_name n1 n2 -> n1 = n2.
Proof.
 intros n1 n2. 
 destruct n1.  destruct n2.
 destruct s.   destruct s0.
  unfold beq_name. intro. apply true_nat_eq in H. subst. auto.
  unfold beq_name. intro. inversion H.
  destruct s0. 
  unfold beq_name. intro. inversion H.
  

eapply beq_nat_refl in H.
  intro.
 unfold beq_name. 
 intro. destruct H.
 eapply band_conj in H.
 destruct s. destruct n0. 
 unfold band in H. simpl.


unfold band.
 apply true_nat_eq in H. subst. trivial.
Qed.

Hint Resolve true_name_eq.


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

Hint Resolve false_name_neq.

Theorem neq_name_false
 : forall n1 n2
 , n1 <> n2 -> false = beq_name n1 n2.
Proof. 
 intros n1 n2. intro.
 destruct n1. destruct n2.
 unfold beq_name. apply neq_nat_false. contradict H. subst. trivial.
Qed.





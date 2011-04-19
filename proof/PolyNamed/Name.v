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


Definition valname (n:name) : Prop 
 := space_of_name n = SValue.
Hint Unfold valname.


Definition tyname  (n:name) : Prop
 := space_of_name n = SType.
Hint Unfold tyname.


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
 destruct n1. destruct n2.
 destruct s; destruct s0; unfold beq_name; auto.
 rewrite beq_nat_sym. auto.
 rewrite beq_nat_sym. auto.
Qed.
Hint Resolve beq_name_sym.


Lemma true_name_eq 
 : forall n1 n2
 , true = beq_name n1 n2 -> n1 = n2.
Proof.
 intros.
 destruct n1. destruct n2.
 destruct s; destruct s0; 
  unfold beq_name in H;
  try (lets N: beq_nat_eq H; subst; auto);
  try (inversion H).
Qed.
Hint Resolve true_name_eq.


Lemma eq_name_true
 : forall n1 n2
 , n1 = n2 -> true = beq_name n1 n2.
Proof.
 intros. subst. eauto.
Qed.
Hint Resolve eq_name_true.


Theorem false_name_neq
 : forall n1 n2
 , false = beq_name n1 n2 -> n1 <> n2.
Proof. 
 intros.
 unfold not. intro. subst. contradict H. 
 rewrite <- beq_name_refl. unfold not. intro. inversion H.
Qed.
Hint Resolve false_name_neq.


Theorem neq_name_false
 : forall n1 n2
 , n1 <> n2 -> false = beq_name n1 n2.
Proof. 
 intros.
 destruct n1. destruct n2.
 destruct s;  destruct s0;
  try (unfold beq_name; auto).

 apply neq_nat_false. contradict H. subst. auto.
 apply neq_nat_false. contradict H. subst. auto.
Qed.


(* Comparison of name spaces ******************************)
Lemma name_valty_false
 :  forall n
 ,  valname n -> tyname n -> False.
Proof.
 intros.
 unfold valname in H.
 unfold tyname in H0.
 rewrite H in H0.
 inversion H0.
Qed.
Hint Resolve   name_valty_false.
Hint Immediate name_valty_false.


Lemma name_valty_neq
 :  forall a b
 ,  valname a -> tyname b -> a <> b.
Proof.
 intros. unfold not. intro. subst.
 eapply name_valty_false. eauto. auto.
Qed.
Hint Resolve   name_valty_neq.
Hint Immediate name_valty_neq.


Lemma name_tyval_neq
 :  forall a b
 ,  tyname a -> valname b -> a <> b.
Proof.
 intros. unfold not. intro. subst.
 eapply name_valty_false. eauto. auto.
Qed.
Hint Resolve   name_tyval_neq.
Hint Immediate name_tyval_neq.



(* Example Names ******************************************)
Notation xA := (Name SValue 0).
Notation xB := (Name SValue 1).
Notation xC := (Name SValue 2).


(* Ordering and boolean comparison library.
   Using the Ordering type is often cleaner than relying on
   nested if-then-else expressions. 
 *)
Require Import BaseTactics.
Require Import BaseNat.

(* Comparison operators *********************************************)
Inductive Ordering :=
 | EQ
 | LT
 | GT.

Fixpoint compare (n1: nat) (n2: nat) : Ordering :=
 match n1, n2 with
 | O,    S _  => LT
 | O,    O    => EQ
 | S _,  O    => GT
 | S m1, S m2 => compare m1 m2
 end.
Hint Unfold compare.


(* Convert ordering to Prop of naturals *****************************)
Theorem compare_eq 
 : forall n m
 , EQ = compare n m -> n = m.
Proof.
 intro.
 induction n.
 intros.
 destruct m.
 auto. 
 simpl in H. inversion H.
 intros.
 destruct m. inversion H.
 simpl in H. apply IHn in H. subst. auto.
Qed.
Hint Resolve compare_eq.


Theorem compare_lt
 : forall n m
 , LT = compare n m -> n < m.
Proof.
 intros. gen m.
 induction n; intros.
 destruct m.
  inversion H. auto.
  destruct m. false.
  apply IHn in H. omega.
Qed.
Hint Resolve compare_lt.


Theorem compare_gt
 : forall n m
 , GT = compare n m -> n > m.
Proof.
 intros. gen n m.
 induction n; intros.
 destruct m. 
  inversion H.
  simpl in H. inversion H.
  destruct m. omega.
  inversion H. apply IHn in H1. omega.
Qed.
Hint Resolve compare_gt.

(* Tactics **********************************************************)
(* Break an of the form (compare ?E1 ?E2) into the possible orderings
      and substitute the ?E1 = ?E2 when they are equal.
*)
Tactic Notation "break_compare" constr(E1) constr(E2) :=
 let X := fresh "X" 
 in  remember (compare E1 E2) as X; destruct X;     
      [ match goal with 
         | [ H: EQ = compare E1 E2 |- _ ] 
         => apply compare_eq in H; subst
        end
      | match goal with 
         | [ H: LT = compare E1 E2 |- _ ] => apply compare_lt in H
        end 
      | match goal with
         | [ H: GT = compare E1 E2 |- _ ] => apply compare_gt in H
        end
      ].

(* Find an expression of the form (compare ?E1 ?E2)
 	and call break_compare on it. 
*)
Tactic Notation "fbreak_compare" := 
 match goal with 
  | [ |- context[compare ?E1 ?E2] ] 
    => break_compare E1 E2
 end.


(* equality *********************************************************)
Definition beq_nat (n1 n2: nat) : bool :=
 match compare n1 n2 with
 | EQ => true
 | _  => false
 end.
Hint Unfold beq_nat.


(* less than equal **************************************************)
Definition ble_nat (n1 n2: nat) : bool :=
 match compare n1 n2 with
 | LT  => true
 | EQ  => true
 | _   => false
 end.
Hint Unfold ble_nat.


Theorem ble_nat_true
 : forall n m 
 , true = ble_nat n m -> n <= m.
Proof.
 intros. generalize dependent m. induction n.
 intros.
 destruct m.
  auto. auto.
  intros. destruct m. inversion H.
  unfold ble_nat in H. breaka (compare (S n) (S m)).
   apply compare_eq in HeqX. omega.
   apply compare_lt in HeqX. omega.
   inversion H.
Qed.
Hint Resolve ble_nat_true.


(* greater than *****************************************************)
Definition bgt_nat (n1 n2: nat) : bool :=
 match compare n1 n2 with
 | GT => true
 | _  => false
 end.
Hint Unfold bgt_nat.


(* greater than equal ***********************************************)
Definition bge_nat (n1 n2: nat) : bool :=
 match compare n1 n2 with
 | GT  => true
 | EQ  => true
 | _   => false
 end.
Hint Unfold bge_nat.


Theorem bge_nat_true 
 : forall n m
 , true = bge_nat n m -> n >= m.
Proof.
 intros. gen m. 
 induction n; intros.
 destruct m. auto. inversion H.
 destruct m. omega.
 unfold bge_nat in H. breaka (compare (S n) (S m)).
  apply compare_eq in HeqX. omega.
  inversion H.
  apply compare_gt in HeqX. omega.
Qed.
Hint Resolve bge_nat_true.


(* less than ********************************************************)
Definition blt_nat (n1 n2: nat) : bool :=
 match compare n1 n2 with
 | LT => true
 | _  => false
 end.
Hint Unfold blt_nat.


Theorem blt_nat_true
 : forall n m
 , true = blt_nat n m -> n < m.
Proof.
 intros. gen m.
 induction n; intros.
 destruct m. inversion H.
 omega.
 destruct m. inversion H.
 unfold blt_nat in H. breaka (compare (S n) (S m)).
  false. false.
Qed.
Hint Resolve blt_nat_true.


Lemma blt_nat_true'
 :  forall n m
 ,  n < m -> true = blt_nat n m.
Proof.
 intros.
 destruct n. destruct m.
 false. omega.

 unfold blt_nat. fbreak_compare; auto.
  false.
  false. omega.

 unfold blt_nat. fbreak_compare; auto.
  false. omega.
  false. omega.
Qed.
Hint Resolve blt_nat_true'.


Lemma bge_nat_false_is_blt
 :  forall n m
 ,  false = bge_nat n m
 -> true  = blt_nat n m.
Proof.
 intros.
 unfold bge_nat in H.
 remember (compare n m).
 destruct o.
  false.
  apply blt_nat_true'. apply compare_lt in Heqo. auto.
  false.
Qed.
Hint Resolve bge_nat_false_is_blt.


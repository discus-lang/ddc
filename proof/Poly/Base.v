
Require Export LibTactics.
Require Export Omega.


(* Tactics ************************************************)
Tactic Notation "break" constr(E) :=
 let X := fresh "X" in remember (E) as X; destruct X.

Tactic Notation "breaka" constr(E) :=
 let X := fresh "X" in remember (E) as X; destruct X; auto.


(* Cases library due to Aaron Bohannon ********************)
Require String. Open Scope string_scope.

Ltac move_to_top x :=
  match reverse goal with
  | H : _ |- _ => try move x after H
  end.

Tactic Notation "assert_eq" ident(x) constr(v) :=
  let H := fresh in
  assert (x = v) as H by reflexivity;
  clear H.

Tactic Notation "Case_aux" ident(x) constr(name) :=
  first [
    set (x := name); move_to_top x
  | assert_eq x name; move_to_top x
  | fail 1 "because we are working on a different case" ].

Tactic Notation "Case" constr(name) := Case_aux Case name.
Tactic Notation "SCase" constr(name) := Case_aux SCase name.
Tactic Notation "SSCase" constr(name) := Case_aux SSCase name.
Tactic Notation "SSSCase" constr(name) := Case_aux SSSCase name.
Tactic Notation "SSSSCase" constr(name) := Case_aux SSSSCase name.
Tactic Notation "SSSSSCase" constr(name) := Case_aux SSSSSCase name.
Tactic Notation "SSSSSSCase" constr(name) := Case_aux SSSSSSCase name.
Tactic Notation "SSSSSSSCase" constr(name) := Case_aux SSSSSSSCase name.


(* Naturals *********************************************************)
Theorem nat_zero_le_all
 : forall n
 , 0 <= n.
Proof.
 intros. omega.
Qed.
Hint Resolve nat_zero_le_all.


Theorem nat_zero_lt_succ
 : forall n
 , 0 < S n.
Proof.
 intros. omega.
Qed.
Hint Resolve nat_zero_lt_succ.


Theorem nat_trans_le
 : forall a b c
 , a <= b -> b <= c -> a <= c.
Proof.
 intros. omega.
Qed. 


Theorem nat_plus_zero
 : forall a
 , a + 0 = a.
Proof. auto. Qed.


Theorem nat_minus_zero
 : forall a
 , a - 0 = a.
Proof. intros. omega. Qed.


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


Theorem compare_lt
 : forall n m
 , LT = compare n m -> n < m.
Proof.
 intros. gen m.
 induction n; intros.
 destruct m.
  inversion H. auto.
  destruct m. inversion H.
  simpl in H. apply IHn in H. omega.
Qed.


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


Definition beq_nat (n1 n2: nat) : bool :=
 match compare n1 n2 with
 | EQ => true
 | _  => false
 end.
Hint Unfold beq_nat.


(* less than equal ****************************************)
Definition ble_nat (n1 n2: nat) : bool :=
 match compare n1 n2 with
 | LT  => true
 | EQ  => true
 | _   => false
 end.


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


(* greater than *****************************************************)
Definition bgt_nat (n1 n2: nat) : bool :=
 match compare n1 n2 with
 | GT => true
 | _  => false
 end.
Hint Unfold bgt_nat.


(* greater than equal **********************************************)
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


(* less than **********************************************)
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
  inversion H.
  apply compare_lt in HeqX. auto.
  inversion H.
Qed.


(* Tactics ************************************************)

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








(* Basic definitions shared by all modules *)

Require Export LibTactics.
Require Export Coq.Arith.EqNat.

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


(* Nats ***************************************************)
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

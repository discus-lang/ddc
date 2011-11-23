(* Add some trivial facts about nats to the auto hint database,
   so we don't have to use omega as much. *)
Require Import DDC.Base.Tactics.

Lemma nat_zero_le_all
 : forall n, 0 <= n.
Proof.
 intros. omega.
Qed.
Hint Resolve nat_zero_le_all.


Lemma nat_zero_lt_succ
 : forall n, 0 < S n.
Proof.
 intros. omega.
Qed.
Hint Resolve nat_zero_lt_succ.

(* Don't add transitivity lemmas to the hints database as it
   can severley degrade performance. *)
Lemma nat_trans_le
 : forall a b c
 , a <= b -> b <= c -> a <= c.
Proof.
 intros. omega.
Qed. 


(* Normalise naturals to use successor representation instead
   of addition. *)
Lemma nat_plus_zero
 : forall n, n + 0 = n.
Proof. auto. Qed.
Hint Rewrite nat_plus_zero : global.


Lemma nat_zero_plus 
 :  forall n, 0 + n = n.
Proof. auto. Qed.
Hint Rewrite nat_zero_plus : global.


Lemma nat_minus_zero
 : forall n, n - 0 = n.
Proof. intros. omega. Qed.
Hint Rewrite nat_minus_zero : global.


Lemma nat_plus_one
 : forall n, n + 1 = S n.
Proof. intros. omega. Qed.


(* Tactics **********************************************************)
(* Normalise naturals. *)
Tactic Notation "nnat" 
 := try rewrite nat_plus_zero
  ; try rewrite nat_minus_zero
  ; try rewrite nat_plus_one.


(* Convert boolean (in)equalities *)
Ltac eqs_beq_nat
 := repeat match goal with
    | [ H : true = beq_nat ?n ?m |- _]
    => symmetry in H; apply beq_nat_true in H
   
    | [H : false = beq_nat ?n ?m |- _]
    => symmetry in H; apply beq_nat_false in H
    end.


(* Break on boolean equality *)
Ltac break_beq_nat
 := match goal with 
     |  [ |- context [beq_nat ?n ?m] ]
     => let X := fresh in remember (beq_nat n m) as X; destruct X
    end; eqs_beq_nat.


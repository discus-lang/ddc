
Require Import BaseTactics.

(* Naturals *********************************************************)
Lemma nat_zero_le_all
 : forall n
 , 0 <= n.
Proof.
 intros. omega.
Qed.
Hint Resolve nat_zero_le_all.


Lemma nat_zero_lt_succ
 : forall n
 , 0 < S n.
Proof.
 intros. omega.
Qed.
Hint Resolve nat_zero_lt_succ.


Lemma nat_trans_le
 : forall a b c
 , a <= b -> b <= c -> a <= c.
Proof.
 intros. omega.
Qed. 


Lemma nat_plus_zero
 : forall a
 , a + 0 = a.
Proof. auto. Qed.


Lemma nat_minus_zero
 : forall a
 , a - 0 = a.
Proof. intros. omega. Qed.


Lemma nat_plus_one
 : forall n, n + 1 = S n.
Proof. intros. omega. Qed.


(* Normalise naturals. *)
Tactic Notation "nnat" 
 := try rewrite nat_plus_zero
  ; try rewrite nat_minus_zero
  ; try rewrite nat_plus_one.

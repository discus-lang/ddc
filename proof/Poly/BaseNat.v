
Require Import BaseTactics.

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

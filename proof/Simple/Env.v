
Require Import Base.

(* option *************************************************)
Inductive option (a : Type) :=
 | none  : option a
 | some  : a -> option a.

Implicit Arguments none [[a]].
Implicit Arguments some [[a]].


(** Environments ******************************************)
Inductive env (A: Type) : Type :=
 | empty  : env A
 | snoc   : env A -> A -> env A.
Hint Constructors env.

Implicit Arguments empty [A].
Implicit Arguments snoc  [A].


Fixpoint cons   {A: Type} (x: A) (e: env A) : env A :=
 match e with
 | empty      => snoc empty x
 | snoc e' y  => snoc (cons x e') y
 end.

Implicit Arguments cons  [A].
Infix ":>" := snoc (at level 61, left  associativity).
Infix "<:" := cons (at level 62, right associativity).


Fixpoint length {A: Type} (e: env A) : nat :=
 match e with 
 | empty      => 0
 | snoc e' x  => S (length e')
 end.
Hint Unfold length.


Fixpoint get {A: Type} (e: env A) (i: nat) : option A :=
 match e, i with
 | snoc _ T,  O    => some T
 | snoc xs _, S i' => get  xs i'
 | _, _            => none
 end.


Fixpoint take {A: Type} (n: nat) (e: env A) : env A :=
 match n, e with
 | O,   _       => empty
 | S n, e' :> T => take n e' :> T
 | S n, empty   => empty
 end.
Hint Unfold take.


(* Lemmas ***********************************************************)
Lemma env_snoc_cons
 :  forall A (e: env A) (x: A) (y: A)
 ,  ((x <: e) :> y) = (x <: (e :> y)).
Proof.
 intros. destruct e.
 simpl. auto.
 simpl. auto.
Qed.




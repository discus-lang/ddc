
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

Implicit Arguments empty [A].
Implicit Arguments snoc  [A].
Infix ":>" := snoc (at level 61, left associativity).


Fixpoint length {A: Type} (e: env A) : nat :=
 match e with 
 | empty      => 0
 | snoc e' x  => S (length e')
 end.

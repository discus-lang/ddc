
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
 | O,   _          => empty
 | S n, e' :> T    => take n e' :> T
 | S n, empty      => empty
 end.
Hint Unfold take.


Fixpoint drop {A: Type} (n: nat) (e: env A) : env A :=
 match n, e with
  | _,     empty   => empty
  | O,     e :> T  => e
  | S n',  e :> T  => drop n' e :> T
  end.


(* Lemmas ***********************************************************)
Theorem cons_snoc_empty
 :  forall A (x: A)
 ,  x <: empty = empty :> x.
Proof.
 intros. 
 unfold cons. auto.
Qed.


Lemma env_snoc_cons
 :  forall A (e: env A) (x: A) (y: A)
 ,  ((x <: e) :> y) = (x <: (e :> y)).
Proof.
 intros. destruct e.
 simpl. auto.
 simpl. auto.
Qed.


Theorem get_weaken1
 :  forall A (e: env A) n x1 x2
 ,  get e n         = some x1
 -> get (x2 <: e) n = some x1.
Proof.
 intros. gen n.
 induction e.
   intros. admit.
   intros.
    destruct n. simpl in H. simpl. auto. 
    simpl. simpl in H. apply IHe. auto.
Qed.


Theorem drop_rewind
 : forall A ix (e : env A) x
 , drop ix e :> x = drop (S ix) (e :> x).
Proof.
 intros. simpl. auto.
Qed.




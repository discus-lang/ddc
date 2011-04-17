
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

Fixpoint append {A: Type} (e1: env A) (e2: env A) : env A :=
 match e2 with 
 | empty      => e1 
 | snoc e2' x => snoc (append e1 e2') x
 end.

Implicit Arguments cons  [A].
Infix ":>" := snoc   (at level 61, left  associativity).
Infix "<:" := cons   (at level 62, right associativity).
Infix "++" := append (at level 63).


Fixpoint length {A: Type} (e: env A) : nat :=
 match e with 
 | empty      => 0
 | snoc e' x  => S (length e')
 end.
Hint Unfold length.


(* Get a numbered element from a list *)
Fixpoint get {A: Type} (e: env A) (i: nat) : option A :=
 match e, i with
 | snoc _ T,  O    => some T
 | snoc xs _, S i' => get  xs i'
 | _, _            => none
 end.


(* Take some elements from the front of a list *)
Fixpoint take {A: Type} (n: nat) (e: env A) : env A :=
 match n, e with
 | O,   _          => empty
 | S n, e' :> T    => take n e' :> T
 | S n, empty      => empty
 end.
Hint Unfold take.


(* Drop a numbered element from a list *)
Fixpoint drop {A: Type} (n: nat) (e: env A) : env A :=
 match n, e with
  | _,     empty   => empty
  | O,     e' :> T  => e'
  | S n',  e' :> T  => drop n' e' :> T
  end.


(* Lemmas ***********************************************************)
Lemma cons_snoc_empty
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


Lemma append_empty
 :  forall A  (e1: env A)
 ,  empty ++ e1 = e1.
Proof.
 intros.
 induction e1. auto. 
 simpl. rewrite IHe1. auto.
Qed.


Lemma append_snoc
 :  forall A  (e1: env A) (e2: env A) (x : A)
 ,  ((e1 :> x) ++ e2) =  e1 ++ (x <: e2).
Proof. 
 intros.
 induction e2.
  auto. 
  simpl. rewrite IHe2. auto.
Qed.


Lemma get_weaken1
 :  forall A (e: env A) n x1 x2
 ,  get e n         = some x1
 -> get (x2 <: e) n = some x1.
Proof.
 intros. gen n.
 induction e.
   intros. 
    destruct n.
     simpl in H. inversions H.
     simpl in H. inversions H.
   intros.
    destruct n. simpl in H. simpl. auto. 
    simpl. simpl in H. apply IHe. auto.
Qed.


Lemma get_weaken 
 :  forall A (e1: env A) (e2: env A) n x1
 ,  get e1 n         = some x1 
 -> get (e2 ++ e1) n = some x1.
Proof.
 intros. gen e1.
 induction e2. 
  intros. rewrite append_empty. auto.
  intros. rewrite append_snoc.
   eapply IHe2. apply get_weaken1. auto.
Qed.


Lemma env_length_zero
 :  forall A (e1: env A)
 ,  length e1 = O -> e1 = empty.
Proof.
 intros.
 destruct e1.
  auto.
  simpl in H. inversions H.
Qed.


Lemma get_succ
 :  forall A n x (e1: env A)
 ,  get (e1 :> x) (S n) = get e1 n.
Proof.
 intros.
 simpl.
 auto.
Qed.


Lemma get_take1
 :  forall A n (e1: env A)
 ,  get (take (S n) e1) n = get e1 n.
Proof.
 intros. gen n.
 induction e1.
  simpl. auto.
  destruct n.
   simpl. auto.
   rewrite get_succ.
   rewrite <- IHe1.
   simpl. auto.
Qed.


Lemma get_cut
 :  forall A (e1: env A) (a: A) n
 ,  get (e1 :> a) (S n) = get e1 n.
Proof. 
 intros. simpl. auto.
Qed.


Lemma get_take 
 :  forall A m n (e1: env A) (x: A)
 ,  m > n -> get e1 n = some x -> get (take m e1) n = some x.
Proof.
 intros. gen n e1.
 induction m.
  intros. inversions H.
  intros. induction n.
   destruct e1. 
    inversions H0. 
    simpl in H0. inversions H0.
    simpl. auto. 
   destruct e1.
    simpl in H0. inversions H0.
    simpl. apply IHm. omega. simpl in H0. auto.
Qed.


Lemma get_drop
 :  forall A n m (e1: env A) x
 ,  m > n -> get e1 n = some x -> get (drop m e1) n = some x.
Proof.
 intros. gen n e1.
 induction m.
  intros. inversions H.
  intros. induction n.
   destruct e1.
    inversions H0.
    simpl in H0. inversions H0.
    simpl. auto.
   destruct e1.
    simpl in H0. inversions H0.
    simpl. apply IHm. omega. simpl in H0. auto.
Qed. 


Lemma drop_rewind
 : forall A ix (e : env A) x
 , drop ix e :> x = drop (S ix) (e :> x).
Proof.
 intros. simpl. auto.
Qed.


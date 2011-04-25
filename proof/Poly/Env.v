(** Environments ****************************************************
  Environments are lists that grow from the right.
  For example:
     Snoc (Snoc (Snoc Empty x3) x2) x1

  This can also be written as:
     Empty :> x3 :> x2 :> x1

  Where :> is sugar for Snoc.
 *)
Require Import Base.


(* Environments. *)
Inductive env (A: Type) : Type :=
 | Empty  : env A
 | Snoc   : env A -> A -> env A.
Hint Constructors env.

Implicit Arguments Empty [A].
Implicit Arguments Snoc  [A].
Infix ":>" := Snoc   (at level 61, left  associativity).


(* Get the length of an environment. *)
Fixpoint length {A: Type} (e: env A) : nat :=
 match e with 
 | Empty      => 0
 | Snoc e' x  => S (length e')
 end.
Hint Unfold length.


(* Add an element to the left of an environment *)
Fixpoint cons   {A: Type} (x: A) (e: env A) : env A :=
 match e with
 | Empty      => Snoc Empty x
 | Snoc e' y  => Snoc (cons x e') y
 end.
Hint Unfold cons.
Implicit Arguments cons  [A].
Infix "<:" := cons   (at level 62, right associativity).


(* Append two environments. *)
Fixpoint append {A: Type} (e1: env A) (e2: env A) : env A :=
 match e2 with 
 | Empty      => e1 
 | Snoc e2' x => Snoc (append e1 e2') x
 end.
Hint Unfold append.
Infix "++" := append.


(* Get an indexed element from a list, starting from 0. *)
Fixpoint get {A: Type} (e: env A) (i: nat) : option A :=
 match e, i with
 | Snoc _ T,  O    => Some T
 | Snoc xs _, S i' => get  xs i'
 | _, _            => None
 end.
Hint Unfold length.


(* Select elements that match a given predicate. *)
Fixpoint filter {A: Type} (f: A -> bool) (e: env A) : env A :=
  match e with 
  | Empty     => Empty
  | Snoc es x 
  => if f x then Snoc (filter f es) x
            else filter f es
  end.


(* Get the nth element from an environment that matches a predicate. *)
Fixpoint getMatch {A: Type} 
            (f: A -> bool) (e: env A) (i: nat) : option A :=
  match e, i with
  | Snoc es x, O     
  => if f x then Some x 
            else getMatch f es O

  | Snoc es x, S i'
  => if f x then getMatch f es i'
            else getMatch f es i
  | _, _
  => None
 end.
Hint Unfold getMatch.


(* Insert a new element at an index in the list, 
   all the elements above that point are shifted up one place. *)
Fixpoint insert {A: Type} (ix: nat) (x: A) (e: env A) : env A 
 := match ix, e with
    | _,     Empty     => Snoc Empty x
    | S ix', Snoc e' y => Snoc (insert ix' x e') y
    | O    , es        => Snoc es x
    end.
Hint Unfold insert.


(* Take some elements from the front of an element. *)
Fixpoint take {A: Type} (n: nat) (e: env A) : env A :=
 match n, e with
 | O,   _           => Empty
 | S n, e' :> T     => take n e' :> T
 | S n, Empty       => Empty
 end.
Hint Unfold take.


(* Drop an indexed element from an environment.
   The resulting environment is one smaller. *)
Fixpoint drop {A: Type} (n: nat) (e: env A) : env A :=
 match n, e with
  | _,     Empty    => Empty
  | O,     e' :> T  => e'
  | S n',  e' :> T  => drop n' e' :> T
  end.
Hint Unfold drop.


(* Lemmas ***********************************************************)
Lemma cons_snoc_empty
 :  forall A (x: A)
 ,  x <: Empty = Empty :> x.
Proof.
 intros. 
 unfold cons. auto.
Qed.
Hint Resolve cons_snoc_empty.


Lemma snoc_cons
 :  forall A (e: env A) (x: A) (y: A)
 ,  ((x <: e) :> y) = (x <: (e :> y)).
Proof.
 intros. destruct e; auto.
Qed.
Hint Resolve snoc_cons.


(* length lemmas ****************************************************)
 Lemma length_zero_is_empty
 :  forall A (e1: env A)
 ,  length e1 = O -> e1 = Empty.
Proof.
 intros.
 destruct e1.
  auto. false.
Qed.
Hint Resolve length_zero_is_empty.


Lemma get_length_more
 :  forall A n (e1: env A) x
 ,  get e1 n = Some x -> length e1 > n.
Proof.
 intros. gen e1.
 induction n.
  intros. destruct e1.
   false.
   simpl in H. inversions H. simpl. omega.
   
  intros. destruct e1.
   false.
   simpl in H. apply IHn in H. simpl. omega.
Qed.
Hint Resolve get_length_more.


(* append lemmas ****************************************************)
Lemma append_empty_left
 :  forall A (e1: env A)
 ,  Empty ++ e1 = e1.
Proof.
 intros.
 induction e1.
  auto. 
  simpl. rewrite IHe1. auto.
Qed.
Hint Resolve append_empty_left.


Lemma append_empty_right
 :  forall A (e1: env A)
 ,  e1 ++ Empty = e1.
Proof. auto. Qed.
Hint Resolve append_empty_right.


Lemma append_snoc
 :  forall A  (e1: env A) (e2: env A) (x : A)
 ,  ((e1 :> x) ++ e2) = e1 ++ (x <: e2).
Proof. 
 intros.
 induction e2.
  auto. 
  simpl. rewrite IHe2. auto.
Qed.
Hint Resolve append_snoc.


(* get lemmas *******************************************************)
Lemma get_succ
 :  forall A n x (e1: env A)
 ,  get (e1 :> x) (S n) = get e1 n.
Proof. auto. Qed.
Hint Resolve get_succ.


Lemma get_succ_some
 :  forall A (e1: env A) n
 ,  (exists t, get e1 (S n) = Some t)
 -> (exists t, get e1 n     = Some t).
Proof.
 intros. gen n.
 induction e1; intros.
  eauto.
  destruct n; simpl; eauto.
Qed.
Hint Resolve get_succ_some.


Lemma get_minus1
 :  forall A n a (e1: env A)
 ,  n > 0
 -> get (e1 :> a) n = get e1 (n - 1).
Proof.
 intros. destruct n.
  inversions H.
  simpl. assert (n - 0 = n). omega. rewrite H0. auto.
Qed.
Hint Resolve get_minus1.


Lemma get_cons_some
 :  forall A (e: env A) n x1 x2
 ,  get e n         = Some x1
 -> get (x2 <: e) n = Some x1.
Proof.
 intros. gen n.
 induction e.
   intros. 
    destruct n.
     simpl in H. false.
     simpl in H. false.
   intros.
    destruct n. simpl in H. simpl. auto. 
    simpl. simpl in H. apply IHe. auto.
Qed.
Hint Resolve get_cons_some.


Lemma get_append_some
 :  forall A (e1: env A) (e2: env A) n x1
 ,  get e1 n         = Some x1 
 -> get (e2 ++ e1) n = Some x1.
Proof.
 intros. gen e1.
 induction e2. 
  intros. rewrite append_empty_left. auto.
  intros. rewrite append_snoc.
   eapply IHe2. apply get_cons_some. auto.
Qed.
Hint Resolve get_append_some.


Theorem get_above_false
 :  forall A n (e1: env A) t
 ,  n >= length e1 
 -> get e1 n = Some t 
 -> False.
Proof.
 intros. gen n t.
 induction e1; intros.
  simpl in H0. false.
  destruct n.
   simpl in H0. inverts H0. inverts H.
   simpl in H0. simpl in H.
   assert (n >= length e1). omega.
   eapply IHe1 in H1. auto. eauto.
Qed.
Hint Resolve get_above_false.


(* filter lemmas ****************************************************)
Lemma filter_length
 :  forall A (e: env A) (f: A -> bool)
 ,  length e >= length (filter f e).
Proof.
 intros. induction e; auto.
 simpl. breaka (f a). simpl. omega.
Qed.


(* getMatch lemmas **************************************************)
Lemma getMatch_filter
 :  forall A (e: env A) (f: A -> bool) n
 ,  getMatch f e n = get (filter f e) n.
Proof.
 intros. gen n.
 induction e; intros.

 Case "Empty".
  auto.

 Case "Snoc".
  simpl. breaka (f a); destruct n; auto.
  simpl. rewrite IHe. auto.
Qed.


Lemma getMatch_cons_some
 :  forall A (e: env A) (f: A -> bool) n x1 x2
 ,  getMatch f e n         = Some x1
 -> getMatch f (x2 <: e) n = Some x1.
Proof.
 intros. gen n.
 induction e; intros; destruct n; simpl in H; simpl.
  false.
  false.
  breaka (f a).
  breaka (f a).
Qed.
Hint Resolve getMatch_cons_some.


Lemma getMatch_above_false
 :  forall A n (e: env A) (f: A -> bool) x
 ,  n >= length e
 -> getMatch f e n = Some x
 -> False.
Proof.
 intros.
 rewrite getMatch_filter in H0.
 assert (length e >= length (filter f e)). apply filter_length.
 assert (n        >= length (filter f e)). omega.
 eapply get_above_false; eauto.
Qed.
Hint Resolve getMatch_above_false.


(* insert lemmas ****************************************************)
Lemma insert_rewind
 :  forall {A: Type} ix t1 t2 (e: env A)
 ,  insert ix t2 e :> t1 = insert (S ix) t2 (e :> t1).
Proof. auto. Qed.


Lemma get_insert_above
 :  forall {A: Type} n ix (e: env A) x1 x2
 ,  n >= ix
 -> get e n                    = Some x1
 -> get (insert ix x2 e) (S n) = Some x1.
Proof.
 intros. gen n e.
 induction ix; intros.
  destruct e.
   false.
   destruct n.
    simpl in H0. auto.
    simpl in H0. auto.
  destruct e.
   false.
   destruct n.
    false. omega.
    simpl in H0. simpl. apply IHix. omega. auto.
Qed.
Hint Resolve get_insert_above.


Lemma get_insert_below
 :  forall {A: Type} n ix (e: env A) x1 x2
 ,  n < ix
 -> get e n                 = Some x1
 -> get (insert ix x2 e) n  = Some x1.
Proof.
 intros. gen n e.
 induction ix; intros.
  destruct e.
   false.
   destruct n.
    false. omega.
    false. omega.
  destruct e.
   false.
   destruct n. 
    simpl in H0. auto.
    simpl in H0. simpl. apply IHix. omega. auto.
Qed.
Hint Resolve get_insert_below.


(* take lemmas ******************************************************)
Lemma take_zero
 :  forall A (e1: env A)
 ,  Empty = take O e1.
Proof.
 intros. auto.
Qed.
Hint Resolve take_zero.


Lemma get_take_succ
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
   auto.
Qed.
Hint Resolve get_take_succ.


Lemma get_take 
 :  forall A m n (e1: env A) (x: A)
 ,  m > n 
 -> get e1 n          = Some x 
 -> get (take m e1) n = Some x.
Proof.
 intros. gen n e1.
 induction m.
  intros. inversions H.
  intros. induction n.
   destruct e1.
    false. 
    simpl in H0. inversions H0. simpl. auto.
   destruct e1.
    false.
    simpl in H0. simpl. apply IHm. omega. auto.
Qed.
Hint Resolve get_take.


Lemma get_take_more
 :  forall A m n (e1: env A) (x : A)
 ,  get (take m e1) n = Some x -> m > n.
Proof.
 intros. gen n e1.
 induction m; intros.
  false.
  destruct e1.
   simpl in H. false.
   simpl in H. destruct n. auto.
   apply IHm in H. omega.
Qed.
Hint Resolve get_take_more.


(* drop lemmas ******************************************************)
Lemma drop_rewind
 : forall A ix (e : env A) x
 , drop ix e :> x = drop (S ix) (e :> x).
Proof.
 intros. simpl. auto.
Qed.
Hint Resolve drop_rewind.


Lemma get_drop_above'
 :  forall A n m (e1: env A) r
 ,  m > n
 -> get e1 n          = r
 -> get (drop m e1) n = r.
Proof.
 intros. gen n e1.
 induction m.
  intros. inversions H.
  intros. induction n.
   destruct e1.
    auto.
    auto.
   destruct e1.
    auto.
    simpl in H0. subst. simpl. apply IHm. omega. auto.
Qed.
Hint Resolve get_drop_above'.


Lemma get_drop_above
 :  forall A n m (e1: env A)
 ,  m > n 
 -> get (drop m e1) n = get e1 n.
Proof.
 intros. breaka (get e1 n); apply get_drop_above'; auto.
Qed.
Hint Resolve get_drop_above.


Lemma get_drop_below'
 :  forall A n m (e1: env A) r
 ,  n >= m
 -> get (drop m e1) n = r
 -> get e1 (S n)      = r.
Proof.
 intros. gen n e1.
 induction m.
  intros. destruct e1.
   auto.
   auto.
  intros. destruct e1.
   auto.
   destruct n.
    inversions H.
    simpl. simpl in H0. apply IHm. omega. auto.
Qed.
Hint Resolve get_drop_below'.


Lemma get_drop_below
 :  forall A n m (e1: env A)
 ,  n >= m
 -> get (drop m e1) n = get e1 (S n).
Proof.
 intros.
 remember (get (drop m e1) n) as r. symmetry.
 eapply get_drop_below'. eauto. auto.
Qed.
Hint Resolve get_drop_below.


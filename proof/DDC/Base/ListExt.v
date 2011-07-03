(* Extensions to the Coq.Lists.List module *)
Require Import DDC.Base.Nat.
Require Import DDC.Base.Tactics.
Require Export Coq.Lists.List.
Require Import Coq.Program.Basics.


(* Unfolding defs from Coq.Lists.List module *)
Hint Unfold length.
Hint Unfold app.
Hint Unfold firstn.
Hint Unfold skipn.


(********************************************************************)
(** Definitions *)

(* Add a single element to the end of the list *)
Fixpoint snoc {A: Type} (x: A) (xx: list A) : list A :=
 match xx with 
 | nil       => x :: nil
 | y :: xs'  => y :: snoc x xs'
 end.
Hint Unfold snoc.


(* Get an indexed element from a list, starting from 0.
   This is like the Coq 'nth' function, but returns an option instead
   of a provided default value. Using an option is useful when we simply
   want to determine whether some element is in the list, but don't
   need the actual value *)
Fixpoint get {A: Type} (i: nat) (e: list A) {struct e}: option A :=
 match e, i with
 | x :: _,  O     => Some x
 | _ :: xs, S i'  => get  i' xs
 | _, _           => None
 end.
Hint Unfold get.


(* Insert a new element at a place in the list.
   All the elements above that place are shifted up one.
   The resulting list is one element larger. *)
Fixpoint insert {A: Type} (ix: nat) (x: A) (xs: list A) : list A :=
 match ix, xs with
 | _,     nil      => x :: nil
 | S ix', y :: xs' => y :: (insert ix' x xs')
 | O    , xs'      => x :: xs'
 end.
Hint Unfold insert.


(* Delete an element at a place in the list.
   All the elements above that place are shifted down one.
   The resulting list is one element smaller. *)
Fixpoint delete {A: Type} (ix: nat) (xs: list A) : list A :=
 match ix, xs with
 | _,    nil       => nil
 | O,    x :: xs'  => xs'
 | S n', x :: xs'  => x :: delete n' xs'
 end.


(* Select elements that match a given predicate. *)
Fixpoint filter {A: Type} (f: A -> bool) (xx: list A) : list A :=
  match xx with 
  | nil     => nil
  | x :: xs
  => if f x then x :: (filter f xs)
            else filter f xs
  end.


(********************************************************************)
(* Environment notations.
   These look more natural than the standard operators when dealing
   with de-Bruijn environments. *)
Notation "xs :> x"  := (x :: xs)   (at level 61, left associativity).
Notation "x  <: xs" := (snoc x xs) (at level 62, right associativity).
Notation "xs >< ys" := (app ys xs) (at level 60, right associativity).


(********************************************************************)
(** Lemmas: cons/snoc *)

Lemma cons_snoc_empty
 :  forall A (x: A)
 ,  x <: nil = nil :> x.
Proof. 
 auto.
Qed.
Hint Resolve cons_snoc_empty.


Lemma snoc_cons
 :  forall A (xs: list A) (x: A) (y: A)
 ,  ((x <: xs) :> y) = (x <: (xs :> y)).
Proof.
 intros. destruct xs; auto.
Qed.
Hint Resolve snoc_cons.


(********************************************************************)
(** Lemmas: length *)

(* A list with length zero is always nil. *)
Lemma length_zero_is_nil
 :  forall A (xx: list A)
 ,  length xx = O 
 -> xx = nil.
Proof.
 intros. 
 destruct xx.
  trivial. false.
Qed.
Hint Resolve length_zero_is_nil.


(* If there is an element at a particular index, then the length
   of the list is bigger than that index. *)
Lemma get_length_more
 :  forall A n (xx: list A) x
 ,  get n xx = Some x 
 -> length xx > n.
Proof.
 intros. gen xx.
 induction n; intros.
  destruct xx.
   false. 
   simpl in H. inverts H. simpl. omega.
   
  destruct xx.
   false.
   simpl in H. apply IHn in H. simpl. omega.
Qed.
Hint Resolve get_length_more.


(********************************************************************)
(** Lemmas: app *)

Lemma app_nil_left
 :  forall A (xx: list A)
 ,  nil ++ xx = xx.
Proof.
 auto.
Qed.
Hint Resolve app_nil_left.


Lemma app_nil_right
 :  forall A (xx: list A)
 ,  xx ++ nil = xx.
Proof. 
 intros.
 induction xx. 
  auto.
  simpl. rewrite IHxx. trivial.
Qed.
Hint Resolve app_nil_right.


Lemma app_snoc
 :  forall A (l1: list A) (l2: list A) (x : A)
 ,  ((l1 :> x) >< l2) = l1 >< (x <: l2).
Proof. 
 intros.
 induction l2.
  auto.
  simpl. rewrite IHl2. auto.
Qed.
Hint Resolve app_snoc.


(********************************************************************)
(** Lemmas: get *)

Lemma get_rewind
 :  forall A n x (xx: list A)
 ,  get n xx = get (S n) (xx :> x).
Proof. auto. Qed.
Hint Resolve get_rewind.


(* If a list contains an element at a non-zero index, 
   then it also contains an element at the previous index. *)
Lemma get_succ_some
 :  forall A (xx: list A) n
 ,  (exists t, get (S n) xx = Some t)
 -> (exists t, get n xx     = Some t).
Proof.
 intros. gen n.
 induction xx; intros.
  simpl in H. inverts H. inverts H0.
  destruct n; simpl; eauto.
Qed.
Hint Resolve get_succ_some.


(* If a list contains an element at a non-zero index, 
   then it also contains an element at the previous index. *)
Lemma get_minus1
 :  forall A n x (xx: list A)
 ,  n > 0
 -> get n (xx :> x) = get (n - 1) xx.
Proof.
 intros. destruct n.
  inverts H.
  simpl. nnat. trivial.
Qed.
Hint Resolve get_minus1.


(* If a list contains an element at a particular index,
   then if we add a new element to the end of the list
   then it still contains the original element at that same index. *)
Lemma get_snoc_some
 :  forall A (xx: list A) n x1 x2
 ,  get n xx         = Some x1
 -> get n (x2 <: xx) = Some x1.
Proof.
 intros. gen n.
 induction xx; intros.
  destruct n.
   simpl in H. false.
   simpl in H. false.
  destruct n. 
   simpl in H. simpl. trivial. 
   simpl in H. simpl. apply IHxx. trivial.
Qed.
Hint Resolve get_snoc_some.


(* If a list contains an element at a particular index,
   then if we append more elements to the end of the ilst
   then it still contains the original element at that same index. *)
Lemma get_append_some
 :  forall A (l1: list A) (l2: list A) n x1
 ,  get n l1         = Some x1 
 -> get n (l1 ++ l2) = Some x1.
Proof.
 intros. gen l1.
 induction l2; intros.
  rewrite app_nil_right. auto.
  rewrite app_snoc. 
   eapply IHl2. apply get_snoc_some. auto.
Qed.
Hint Resolve get_append_some.


(* We cannot get elements from a list at indices the same, or larger, 
   than the length of that list *)
Theorem get_above_false
 :  forall A n (xx: list A) t
 ,  n >= length xx
 -> get n xx = Some t 
 -> False.
Proof.
 intros. gen n t.
 induction xx; intros.
  simpl in H0. false.
  destruct n.
   simpl in H0. inverts H0. inverts H.
   simpl in H0. simpl in H.
   assert (n >= length xx). omega.
   eapply IHxx in H1. false. eauto.
Qed.
Hint Resolve get_above_false.


(********************************************************************)
(** Lemmas: map *)

Lemma map_rewind
 :  forall {A B: Type} (f: A -> B) (e: list A) x
 ,  map f e :> f x 
 =  map f (e :> x).
Proof. auto. Qed.


(* Applying a function to all the elements of a list and getting
   one of the results is the same as getting the original value
   and applying the function to just that value. *)
Lemma get_map 
 : forall {A B: Type} (f: A -> B) (xx: list A) x n
 ,  get n xx         = Some x
 -> get n (map f xx) = Some (f x).
Proof.
 intros. gen n. 
 induction xx; intros.
  false.
  simpl. destruct n.
   simpl in H. inverts H. trivial.
   auto.
Qed.
Hint Resolve get_map.


(* Applying two functions to all the elements of a list one after
   the other is the same as applying their composition. *)
Lemma map_map
 :  forall {A B C: Type} (f: B -> C) (g: A -> B) xx
 ,  map f (map g xx) 
 =  map (compose f g) xx.
Proof.
 induction xx.
  auto.
  simpl. rewrite IHxx. auto.
Qed.


(* Extensional equality with map.
   If two functions return equal results for all elements in a list, 
   then using one or the other in a map gives the same result. *) 
Lemma map_ext_in
 : forall {A B : Type}
          (f g : A -> B)
          (xs  : list A)
 , (forall x, In x xs -> f x = g x)
 -> map f xs = map g xs.
Proof.
 intros.
 induction xs.
  auto.
  simpl. rewrite IHxs. rewrite H.
   auto. simpl. auto. 
   intros. apply H.
   simpl. auto.
Qed.


(* If some element is in the list resulting from a map, 
   then we can find the un-transformed element in the original list. *)
Lemma map_in_exists
  :  forall (A B: Type) (f: A -> B) x ys
  ,  In x (map f ys)
  -> (exists y, f y = x /\ In y ys).
Proof.
 intros.
 induction ys.
  simpl in H. false.
  simpl in H.
   inverts H.
   simpl. exists a. eauto.
   apply IHys in H0.
   destruct H0.
   exists x0. inverts H. split. auto. eauto.
   simpl. eauto.
Qed.


(* When we transform a list with a map, then we can find the element
   in the result corresponding to any element in the source. *)
Lemma map_exists_in
  :  forall (A B: Type) (f: A -> B) x ys
  ,  (exists y, f y = x /\ In y ys)
  -> In x (map f ys).
Proof.
 intros.
 induction ys.
  destruct H.
   simpl in H. inverts H. false.
   simpl in H.
   destruct H.
   inverts H.
   simpl. 
   inverts H1. auto.
   right. eauto.
Qed.


(********************************************************************)
(** Lemmas: firstn *)

(* If we take zero elements from a list,
   then the resulting list is empty *)
Lemma firstn_zero
 :  forall A (xx: list A)
 ,  firstn O xx = nil.
Proof.
 intros. auto.
Qed.
Hint Resolve firstn_zero.


(* Up-lemma for get_take.
   If a list has an element at a particular index 
   if we take the elements from the list at least up to that index
   then the resulting list still has the original element at the
   same point. *)
Lemma get_firstn_succ
 :  forall A n (xx: list A)
 ,  get n (firstn (S n) xx) = get n xx.
Proof.
 intros. gen n.
 induction xx.
  simpl. auto.
  destruct n.
   auto.
   rewrite <- get_rewind. rewrite <- IHxx. auto.
Qed.
Hint Resolve get_firstn_succ.


(* If a list has an element at a particular index 
   if we take the elements from the list at least up to that index
   then the resulting list still has the original element at the
   same point. *)
Lemma get_firstn 
 :  forall A m n (xx: list A) (x: A)
 ,  m > n 
 -> get n xx            = Some x 
 -> get n (firstn m xx) = Some x.
Proof.
 intros. gen n xx.
 induction m; intros.
  inverts H.
  induction n.
   destruct xx.
    false. 
    simpl in H0. inverts H0. auto.
   destruct xx.
    false.
    simpl in H0. simpl. apply IHm. omega. trivial.
Qed.
Hint Resolve get_firstn.


(* If we take some elements from the front of a list,
   and there is still an element at a particular index
   then the number of elements we took was more than that index. *)
Lemma get_firstn_more
 :  forall A m n (xx: list A) (x: A)
 ,  get n (firstn m xx) = Some x -> m > n.
Proof.
 intros. gen n xx.
 induction m; intros.
  false.
  destruct xx.
   false.
   simpl in H. destruct n. 
    auto.
    apply IHm in H. omega.
Qed.
Hint Resolve get_firstn_more.


(********************************************************************)
(** Lemmas: insert *)

Lemma insert_rewind
 :  forall {A: Type} ix t1 t2 (xx: list A)
 ,  insert ix t2 xx :> t1 = insert (S ix) t2 (xx :> t1).
Proof. auto. Qed.


(* If we insert an element at a particular point in a list, 
   then we can still get the elements above that point
   provided we increment their original indices. *)
Lemma get_insert_above
 :  forall {A: Type} n ix (xx: list A) x1 x2
 ,  n >= ix
 -> get n xx                    = Some x1
 -> get (S n) (insert ix x2 xx) = Some x1.
Proof.
 intros. gen n xx.
 induction ix; intros.
  destruct xx.
   false.
   destruct n; auto.
  destruct xx.
   false.
   destruct n.
    false. omega.
    simpl in H0. simpl. apply IHix. 
     omega. 
     auto.
Qed.
Hint Resolve get_insert_above.


(* If we insert an element at a particular point in a list, 
   then we can still get the elements below that point
   using their original indices. *)
Lemma get_insert_below
 :  forall {A: Type} n ix (xx: list A) x1 x2
 ,  n < ix
 -> get n xx                = Some x1
 -> get n (insert ix x2 xx) = Some x1.
Proof.
 intros. gen n xx.
 induction ix; intros.
  destruct xx.
   false.
   destruct n.
    false. omega.
    false. omega.
  destruct xx.
   false.
   destruct n. 
    simpl in H0. auto.
    simpl in H0. simpl. apply IHix. omega. auto.
Qed.
Hint Resolve get_insert_below.


(* Inserting a new element into a list then applying a function to 
   all elements is the same as applying the function to all the
   original elements, then inserting the new one with the function
   already applied. *)
Lemma map_insert 
 : forall {A B: Type} (f: A -> B) ix x (xs: list A)
 , map f (insert ix x xs)
 = insert ix (f x) (map f xs).
Proof.
 intros. gen ix x.
 induction xs; intros.
  simpl. destruct ix; auto.
  simpl. destruct ix; auto.
   rewrite <- insert_rewind. simpl.
   rewrite IHxs. auto.
Qed.


(********************************************************************)
(** Lemmas: delete *)

Lemma delete_rewind
 : forall A ix (xs: list A) x
 , x :: delete ix xs = delete (S ix) (x :: xs).
Proof.
 intros. simpl. auto.
Qed.
Hint Resolve delete_rewind.


(* If we delete an element at a point in a list
   then we can still get the elements below that point
   using their original indices. *)
Lemma get_delete_above'
 :  forall A n m (xx: list A) r
 ,  m > n
 -> get n xx            = r
 -> get n (delete m xx) = r.
Proof.
 intros. gen n xx.
 induction m.
  intros. inversions H.
  intros. induction n.
   destruct xx.
    auto.
    auto.
   destruct xx.
    auto.
    simpl in H0. subst. simpl. apply IHm.
     omega.
     trivial.
Qed.
Hint Resolve get_delete_above'.


Lemma get_delete_above
 :  forall A n m (xx: list A)
 ,  m > n 
 -> get n (delete m xx) = get n xx.
Proof.
 intros. 
 breaka (get n xx); apply get_delete_above'; auto.
Qed.
Hint Resolve get_delete_above.


(* If we delete an element at a point in a list
   then we can still get the elements above that point
   provided we decrement their original indices. *)
Lemma get_delete_below'
 :  forall A n m (xx: list A) r
 ,  n >= m
 -> get n (delete m xx) = r
 -> get (S n) xx        = r.
Proof.
 intros. gen n xx.
 induction m; intros.
  destruct xx; auto.
  destruct xx.
   auto.
   destruct n.
    inverts H.
    simpl. simpl in H0. apply IHm. 
     omega.
     auto.
Qed.
Hint Resolve get_delete_below'.


Lemma get_delete_below
 :  forall A n m (xx: list A)
 ,  n >= m
 -> get n (delete m xx) = get (S n) xx.
Proof.
 intros.
 remember (get n (delete m xx)) as r. 
 symmetry.
 eapply get_delete_below'; eauto.
Qed.
Hint Resolve get_delete_below.


Lemma map_delete
 :  forall {A B: Type} (f: A -> B) ix (xx: list A)
 ,  map f (delete ix xx) 
 =  delete ix (map f xx).
Proof.
 intros. gen ix.
 induction xx.
  simpl. destruct ix; auto.
  simpl. destruct ix; auto.
   rewrite <- delete_rewind.
   rewrite <- delete_rewind.
   rewrite <- IHxx. auto.
Qed.


(********************************************************************)
(* Lemmas: filter *)

(* The length of a filtered list is the same or smaller than
   the original list. *)
Lemma filter_length
 :  forall A (xx: list A) (f: A -> bool)
 ,  length xx >= length (filter f xx).
Proof.
 intros. induction xx; auto.
 simpl. breaka (f a). simpl. omega.
Qed.


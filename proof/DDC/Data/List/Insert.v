
Require Import DDC.Data.List.Base.
Require Import DDC.Base.Nat.
Require Import DDC.Base.Tactics.


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


Lemma insert_app
 : forall {A : Type} ix (x: A) xs ys
 , insert ix x xs >< ys = insert (ix + length ys) x (xs >< ys).
Proof.
 intros. 
 induction ys. 
  simpl. nnat.  auto.
  simpl. rewrite IHys.
   rewrite insert_rewind.
   assert (S (ix + length ys) = ix + S (length ys)). auto.
   rewrite H. auto.
Qed.




Require Import DDC.Data.List.Base.
Require Import DDC.Base.Nat.
Require Import DDC.Base.Tactics.


(* Delete an element at a place in the list.
   All the elements above that place are shifted down one.
   The resulting list is one element smaller. *)
Fixpoint delete {A: Type} (ix: nat) (xs: list A) : list A :=
 match ix, xs with
 | _,    nil       => nil
 | O,    x :: xs'  => xs'
 | S n', x :: xs'  => x :: delete n' xs'
 end.


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


Lemma delete_app
 :  forall A n (e1: list A) (e2: list A)
 ,  delete n e1 >< e2 
 =  delete (n + length e2) (e1 >< e2).
Proof.
 intros.
 induction e2.
  simpl. nnat. auto.
  simpl. rewrite IHe2.
   assert (n + S (length e2) = S (n + length e2)). omega. rewrite H.
   apply delete_rewind.
Qed.


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




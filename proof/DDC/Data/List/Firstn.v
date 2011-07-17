
Require Import DDC.Data.List.Base.
Require Import DDC.Base.Tactics.

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



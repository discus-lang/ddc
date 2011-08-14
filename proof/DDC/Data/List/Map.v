
Require Import DDC.Data.List.Base.
Require Import DDC.Base.Tactics.
Require Import Coq.Program.Basics.


(********************************************************************)
(** Lemmas: map *)

Lemma map_rewind
 :  forall {A B: Type} (f: A -> B) (e: list A) x
 ,  map f e :> f x 
 =  map f (e :> x).
Proof. auto. Qed.


Lemma map_snoc
 :  forall {A B} (f: A -> B) x xs
 ,  map f (snoc x xs) = snoc (f x) (map f xs).
Proof.
 intros.
 induction xs.
  simpl. auto.
  simpl. rewrite IHxs. auto.
Qed.


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


Lemma map_impl
 :  forall {A B} (f: A -> B) {g: A -> B} xs
 ,  (forall x, f x = g x)
 -> map f xs = map g xs.
Proof.
 intros.
 induction xs.
  auto.
  repeat rewritess.
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


Lemma map_get_some_some
 :  forall {A} t1 t2 ix us (f: A -> A)
 ,  Some t1 = get ix us
 -> Some t2 = get ix (map f us)
 -> f t1 = t2.
Proof.
 intros. gen ix t1 t2.
 induction us; intros.
  false.
  destruct ix.
   simpl in H. simpl in H0. 
   inverts H.  inverts H0. auto.

   simpl in H. simpl in H0; eauto.
Qed.


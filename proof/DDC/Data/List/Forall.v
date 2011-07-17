
Require Import DDC.Data.List.Base.
Require Import DDC.Base.Tactics.


(********************************************************************)
(* Lemmas: Forall *)

Lemma Forall_impl_in
 : forall {A: Type}
          (P1: A -> Prop) (P2: A -> Prop)
          (xs: list A)
 ,  (forall x, In x xs -> P1 x -> P2 x)
 -> Forall P1 xs
 -> Forall P2 xs.
Proof.
 intros.
 induction xs.
  auto. 
  inverts H0. intuition.
Qed.


Lemma Forall_map
 :  forall {A B: Type} 
           (P: B -> Prop) (f: A -> B) 
           (xs: list A)
 ,  Forall (fun x => P (f x)) xs
 -> Forall P (map f xs).
Proof.
 intros. induction xs.
  apply Forall_nil.
  inverts H. simpl. intuition.
Qed.


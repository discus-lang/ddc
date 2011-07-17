
Require Import DDC.Data.List.Base.
Require Import DDC.Base.Tactics.


(* Select elements that match a given predicate. *)
Fixpoint filter {A: Type} (f: A -> bool) (xx: list A) : list A :=
  match xx with 
  | nil     => nil
  | x :: xs
  => if f x then x :: (filter f xs)
            else filter f xs
  end.


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

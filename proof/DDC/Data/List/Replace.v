
Require Import DDC.Data.List.Base.
Require Import DDC.Data.Nat.
Require Import DDC.Base.Tactics.


(* Replace the element at point in the list.
   If the list is shorter than expected then do nothing 
 *)
Fixpoint replace {A: Type} (ix: nat) (x': A) (xs: list A) : list A :=
 match ix, xs with
 | _,     nil      => nil
 | S ix', y :: xs' => y  :: (replace ix' x' xs')
 | O    , x :: xs' => x' :: xs'
 end.
Hint Unfold replace.

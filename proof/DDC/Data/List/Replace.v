
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


Lemma replace_nil
 :  forall {A} n (x : A)
 ,  replace n x nil = nil.
Proof.
 intros. destruct n; auto.
Qed.


Lemma replace_length
 : forall {A} n x (xs : list A)
 , length (replace n x xs) = length xs.
Proof.
 intros. gen n.
 induction xs; intros.
  rewrite replace_nil; auto.
  destruct n; burn.
Qed. 


Lemma replace_get_eq
 :  forall {A} (xs: list A) x x' n
 ,  get n xs                  = Some x
 -> get n (replace n x' xs)   = Some x'.
Proof.
 intros. gen n.
 induction xs; intros.
  rewrite replace_nil.
   false.
  destruct n.
   simpl. auto.
   simpl. simpl in H.
   auto.
Qed.
Hint Resolve replace_get_eq.


Lemma replace_get_neq
 :  forall {A} (xs: list A) x n0 n1
 ,  n0 <> n1
 -> get n0 (replace n1 x xs) = get n0 xs.
Proof.
 intros. gen n0 n1.
 induction xs; intros.
  rewrite replace_nil. auto.
  destruct n1.
   simpl.
   destruct n0.
    burn. auto.
   simpl. 
   destruct n0.
    auto. auto.
Qed.
Hint Resolve replace_get_neq.


Lemma replace_id
 :  forall {A} x (xs: list A) i
 ,  get i xs = Some x
 -> replace i x xs = xs.
Proof.
 intros. gen i.
 induction xs; intros.
  rewrite replace_nil. auto.
  destruct i.
   simpl.
   simpl in H. inverts H. auto.
   simpl. rs.
Qed.
Hint Resolve replace_id.


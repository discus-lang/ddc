
Require Import DDC.Data.List.Base.
Require Import DDC.Base.Tactics.


(* Update the element at the given position in a list.
   If the position is not in the list then return the original list. *)
Fixpoint update {A: Type} (ix: nat) (y: A) (xx: list A) : list A :=
 match ix, xx with 
 | _,    nil       => nil
 | O,    x :: xs   => y :: xs
 | S n', x :: xs   => x :: update n' y xs
 end.


Lemma Forall_update
 :  forall A (P: A -> Prop) ix x xs
 ,  P x
 -> Forall P xs
 -> Forall P (update ix x xs).
Proof.
 intros. gen ix.
 induction xs; intros.
  destruct ix; simpl; auto.
  destruct ix; inverts H0.
   simpl. auto.
   simpl. apply Forall_cons; auto.
Qed.


Lemma Forall_update_result
 :  forall A (P: A -> Prop) ix x y xs
 ,  get ix xs = Some y
 -> Forall P (update ix x xs)
 -> P x.
Proof.
 intros. gen xs.
 induction ix; intros.
  destruct xs.
   inverts H.
   simpl in H. inverts H.
   simpl in H0. inverts H0. auto.
  destruct xs.
   simpl in H. false.
   simpl in H.
   simpl in H0. inverts H0.
   eauto.
Qed.

 
Lemma Forall2_update_right
 : forall {A B : Type} 
          (R   : A -> B -> Prop)
          (xs  : list A)
          (ys  : list B)
          ix x y
 ,  R x y
 -> Forall2 R xs ys
 -> get ix ys = Some y
 -> Forall2 R (update ix x xs) ys.
Proof.
 intros. gen ix.
 induction H0; intros.
  false.
  induction ix.
   simpl. simpl in H2. inverts H2. auto.
   simpl. eauto.
Qed.


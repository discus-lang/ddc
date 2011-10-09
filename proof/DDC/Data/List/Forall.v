
Require Import DDC.Data.List.Base.
Require Import DDC.Base.Tactics.


(********************************************************************)
(* Lemmas: in *)
Lemma in_snoc 
 :  forall A x a (xs: list A)
 ,  In x xs 
 -> In x (a <: xs).
Proof.
 intros.
 induction xs.
  nope.
  simpl in H.
   inverts H.
   simpl. auto.
   simpl. eauto.
Qed.
Hint Resolve in_snoc.


Lemma in_app_right
 :  forall A x (xs ys: list A)
 ,  In x ys
 -> In x (xs ++ ys).
Proof.
 intros.
 induction xs.
  rr. auto.
  rrwrite ((a :: xs) ++ ys = a :: (xs ++ ys)).
  apply in_cons. auto.
Qed.
Hint Resolve in_app_right.


Lemma in_app_left
 :  forall A x (xs ys: list A)
 ,  In x xs
 -> In x (xs ++ ys).
Proof.
 intros.
 eapply (@rev_ind A (fun ys => In x (xs ++ ys))); intros.
  rr. auto.
  rr. rrwrite ((x0 <: l) >< xs = (x0 <: (l >< xs))). eauto.
Qed.
Hint Resolve in_app_left.


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


Lemma Forall_get
 :  forall A (P: A -> Prop) ix x xs
 ,  Forall P xs
 -> get ix xs = Some x
 -> P x.
Proof.
 intros. gen x xs.
 induction ix; intros.
  destruct xs.
   false.
   simpl in H0. inverts H0. inverts H. auto.
  destruct xs.
   false.
   simpl in H0.
   eapply IHix.
    inverts H. eapply H4. auto.
Qed.


Lemma Forall_snoc
 :  forall A (P: A -> Prop) x xs
 ,  P x
 -> Forall P xs
 -> Forall P (x <: xs).
Proof.
 intros. gen x.
 induction xs; intros. 
  simpl. auto.
  simpl. 
   eapply Forall_cons. 
    inverts H0. auto.
   eapply IHxs.
    inverts H0. auto. auto.
Qed.
Hint Resolve Forall_snoc.


Lemma Forall_app_left
 :  forall A (P: A -> Prop) xs ys
 ,  Forall P (xs ++ ys)
 -> Forall P xs.
Proof.
 intros. nforall. eauto.
Qed.
Hint Resolve Forall_app_left.


Lemma Forall_app_right
 :  forall A (P: A -> Prop) xs ys
 ,  Forall P (xs ++ ys)
 -> Forall P ys.
Proof.
 intros. nforall. eauto.
Qed.
Hint Resolve Forall_app_right.


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
Hint Resolve Forall_map.

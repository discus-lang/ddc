
Require Export Coq.Lists.List.
Require Import BaseTactics.


(* Get an indexed element from a list, starting from 0. *)
Fixpoint getl {A: Type} (e: list A) (i: nat) : option A :=
 match e, i with
 | T :: _,  O    => Some T
 | _ :: xs, S i' => getl  xs i'
 | _, _          => None
 end.
Hint Unfold getl.


(* map lemmas *******************************************************)
Lemma length_map
 : forall {A B : Type} (f : A -> B) (xs : list A)
 , length xs = length (map f xs).
Proof.
 intros.
 induction xs.
  eauto.
  simpl. eauto.
Qed.


Lemma map_ext_In
 : forall {A B : Type}
          (f g : A -> B)
          (xs  : list A)
 ,  (forall x, In x xs -> f x = g x)
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


Lemma In_map_exists
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


Lemma In_exists_map
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


(* Forall ***********************************************************)
Lemma Forall_impl_In
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


(* Forall2 **********************************************************)
Lemma Forall2_impl
 : forall (A B: Type) 
          (R1: A -> B -> Prop)
          (R2: A -> B -> Prop)
          xs ys
 , (forall x y, R1 x y -> R2 x y)
 -> Forall2 R1 xs ys 
 -> Forall2 R2 xs ys.
Proof.
 intros. induction H0; auto. 
Qed.


Lemma Forall2_impl_In
 : forall {A B: Type}
          (R1: A -> B -> Prop)
          (R2: A -> B -> Prop)
          (xs: list A)
          (ys: list B)
 ,  (forall x y, In x xs -> In y ys -> R1 x y -> R2 x y)
 -> Forall2 R1 xs ys
 -> Forall2 R2 xs ys.
Proof.
 intros.
 induction H0.
  apply Forall2_nil.
  intuition.
Qed.


Lemma Forall2_exists_left
 : forall (A B: Type) (R: A -> B -> Prop) x xs ys
 ,  In x xs 
 -> Forall2 R xs ys 
 -> (exists y, R x y).
Proof.
 intros.
 induction H0.
  false.
  simpl in H. destruct H.
   subst. eauto.
   eapply IHForall2. eauto.
Qed.


Lemma Forall2_exists_right
 : forall (A B: Type) (R: A -> B -> Prop) y xs ys
 ,  In y ys 
 -> Forall2 R xs ys 
 -> (exists x, R x y).
Proof.
 intros.
 induction H0.
  false.
  simpl in H. destruct H.
   subst. eauto.
   eapply IHForall2. eauto.
Qed.


Lemma Forall2_map_left
 : forall {A B C: Type}
          (R1: B -> C -> Prop)
          (f:  A -> B)
          (xs: list A) (ys: list C)
 ,  Forall2 (fun x y => R1 (f x) y) xs ys
 -> Forall2 R1 (map f xs) ys.
Proof.
 intros.
 induction H.
  apply Forall2_nil.
  simpl. intuition.
Qed.


Lemma Forall2_map_right
 : forall {A B C: Type}
          (R1: A -> C -> Prop)
          (f:  B -> C)
          (xs: list A) (ys: list B)
 ,  Forall2 (fun x y => R1 x (f y)) xs ys
 -> Forall2 R1 xs (map f ys).
Proof.
 intros.
 induction H.
  apply Forall2_nil.
  simpl. intuition.
Qed.


Lemma Forall2_Forall_left
 : forall {A B : Type}
          (R   : A -> B -> Prop)
          (P   : A -> Prop)
          (xs  : list A)
          (ys  : list B)
 ,  Forall  (fun x => forall y, R x y -> P x) xs
 -> Forall2 R xs ys
 -> Forall  P xs.
Proof.
 intros.
 rewrite Forall_forall.
 rewrite Forall_forall in H. 
 intros.
 lets D: Forall2_exists_left H1 H0.
 destruct D. eauto. 
Qed.

Hint Resolve Forall_forall.


(*
Lemma Forall_impl_elem
 : forall {A: Type}
          (P Q: A -> Prop)
          (xs:  list A)
  ,  Forall P xs
  -> Forall (fun x => P x -> Q x) xs
  -> Forall Q xs.
Proof.
 admit.
Qed.

Lemma Forall2_Forall_exists
 : forall {A B : Type}
          (R   : A -> B -> Prop)
          (xs  : list A)
          (ys  : list B)
 ,  Forall2 R xs ys
 -> Forall  (fun x => exists y, R x y) xs.
Proof.
 admit.
Qed.
*)

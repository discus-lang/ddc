
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


Lemma Forall_map
 : forall (A B: Type) 
          (P:   B -> Prop)
          (f:   A -> B)
          (xs:  list A)
 ,  Forall (fun x => P (f x)) xs
 -> Forall P (map f xs).
Proof.
 intros. induction xs.
  apply Forall_nil.
  simpl. 
  inverts H.
  apply Forall_cons. auto. auto.
Qed.


Lemma Forall_impl_In
 : forall {A: Type}
          (P1: A -> Prop)
          (P2: A -> Prop)
          (xs: list A)
 ,  (forall x, In x xs -> P1 x -> P2 x)
 -> Forall P1 xs
 -> Forall P2 xs.
Proof.
 admit.
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


Lemma Forall2_map_right
 : forall {A B C: Type}
          (R1: A -> C -> Prop)
          (f:  B -> C)
          (xs: list A) (ys: list B)
 ,  Forall2 (fun x y => R1 x (f y)) xs ys
 -> Forall2 R1 xs (map f ys).
Proof.
 admit.
Qed.

Lemma Forall2_map_left
 : forall {A B C: Type}
          (R1: B -> C -> Prop)
          (f:  A -> B)
          (xs: list A) (ys: list C)
 ,  Forall2 (fun x y => R1 (f x) y) xs ys
 -> Forall2 R1 (map f xs) ys.
Proof.
 admit.
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
 admit.
Qed.



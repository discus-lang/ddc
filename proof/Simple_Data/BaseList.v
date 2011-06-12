
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


Lemma Forall2_length
 : forall {A B: Type} (R: A -> B -> Prop) 
          (xs : list A) 
          (ys : list B)
 ,  Forall2 R xs ys
 -> length xs = length ys.
Proof.
 intros.
 induction H.
  auto.
  simpl. auto.
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


Lemma Forall2_exists_left_In
 : forall (A B: Type) (R: A -> B -> Prop) x xs ys
 ,             In x xs  -> Forall2 R xs ys 
 -> (exists y, In y ys  /\         R x  y).
Proof.
 intros.
 induction H0.
  false.
  simpl in H. destruct H.
   subst.
   exists y. split. simpl. auto. auto.
   lets D: IHForall2 H.
   destruct D.
   exists x1.
    inverts H2.
    split. simpl. auto. auto.
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


(********************************************************************)
Fixpoint take {A: Type} (n: nat) (xx: list A) : list A :=
 match n, xx with
 | O, _           => nil
 | S n', nil      => nil
 | S n', x :: xs  => x :: take n' xs
 end.


Lemma take_length 
 :  forall {A: Type} n (xs: list A)
 ,  n < length xs
 -> length (take n xs) = n.
Proof.
 intros. gen n.
 induction xs; intros.
  inverts H.
  destruct n.
   auto.
   simpl.
   f_equal.
   apply IHxs.
   simpl in H. omega.
Qed.
Hint Resolve take_length.


Lemma take_nil
 :  forall {A: Type} n
 ,  take n (@nil A) = nil.
Proof.
 intros. destruct n; auto.
Qed.
Hint Resolve take_nil.


Lemma take_cons
 :  forall {A: Type} n x (xs: list A) y (ys : list A)
 ,  take n (x :: xs ++ y :: ys) = x :: xs
 -> take n (     xs ++ y :: ys) =      xs ++ y :: nil.
Proof.
 induction n; intros.
 simpl in H.
  inverts H.
 simpl in H.
  inversion H.
  rewrite H1.
  destruct xs.
  simpl. simpl in H1. f_equal.
   destruct n.
    auto.
    simpl in H1. inverts H1.

  simpl. f_equal.
  eapply IHn. eauto.
Qed. 


Lemma take_step
 :  forall {A: Type} n (xx: list A) y (ys: list A)
 ,  take  n    (xx ++ y :: ys) = xx
 -> take (S n) (xx ++ y :: ys) = xx ++ (y :: nil).
Proof.
 intros.
 destruct xx.
  simpl. f_equal. simpl in H.
  destruct n.
   auto.
   simpl in H. inverts H.
  simpl. f_equal. simpl in H.
   apply take_cons in H. auto.
Qed.


(*******************************************************************)
Fixpoint drop {A: Type} (n: nat) (xx: list A) : list A :=
 match n, xx with
 | O, _           => xx
 | S n', nil      => nil
 | S n', x :: xs  => drop n' xs
 end.


Lemma drop_nil
 :  forall {A: Type} n
 ,  drop n (@nil A) = nil.
Proof.
 intros. destruct n; auto.
Qed.


Lemma take_drop
 :  forall {A: Type} n (xx: list A)
 ,  take n xx ++ drop n xx = xx.
Proof.
 intros. gen xx.
 induction n; intros.
  simpl. auto.
  destruct xx.
   simpl. auto.
   simpl. f_equal. auto.
Qed.


Lemma drop_step
 :  forall {A: Type} n (xx: list A) y (ys: list A)
 ,  drop n     xx = y :: ys
 -> drop (S n) xx = ys.
Proof.
 intros. gen n. 
 induction xx; intros.
  rewrite drop_nil in H. inverts H.
  simpl.
  destruct n.
   simpl in H. inverts H. auto.
   simpl in H. auto.
Qed.


(*******************************************************************)
Definition splitAt {A: Type} (ix: nat) (xx: list A) : (list A * list A) :=
 (take ix xx, drop ix xx).
Hint Unfold splitAt.


Lemma splitAt_app 
 :  forall {A: Type} ix (xs: list A) (ys: list A) (zs: list A)
 ,  splitAt ix xs = (ys, zs)
 -> xs = ys ++ zs.
Proof.
 intros.
 unfold splitAt in H.
 inverts H.
 symmetry. apply take_drop.
Qed.


Lemma splitAt_app_cons
 :  forall {A: Type} ix (xs: list A) (yy: list A) (z: A) (zs: list A)
 ,  splitAt ix xs = (yy, z :: zs)
 -> xs = yy ++ (z :: zs).
Proof.
 intros.
 unfold splitAt in H.
 inverts H. 
 symmetry. apply take_drop.
Qed.


Lemma splitAt_shift
 :  forall {A: Type} ix (xs: list A) (ys: list A) (z0: A) (z1: A) (zs: list A)
 ,  splitAt ix     xs = (ys,                z0 :: z1 :: zs)
 -> splitAt (S ix) xs = (ys ++ (z0 :: nil), z1 :: zs).
Proof.
 intros. 
 lets D:  @splitAt_app A.
 lets D1: D H. clear D.
 unfold splitAt.
 unfold splitAt in H. inversion H. clear H.
  rewrite H1.
 rewrite D1 in H1.
 rewrite D1 in H2.
 f_equal. 
  rewrite D1. apply take_step. auto.
  rewrite D1. eapply drop_step. eauto.
Qed.


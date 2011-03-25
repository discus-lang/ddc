(* Typing contexts. *)

Require Import Base.
Require Import Name.
Require Import FunctionalExtensionality.


Definition partial_map (A:Type) 
 := name -> option A.


(* An empty typing context *)
Definition empty {A:Type} : partial_map A 
 := fun _ => none.

Hint Unfold empty.


(* Extend a typing context *)
Definition extend {A:Type} (rest:partial_map A) (x:name) (T:A)
 := fun x' 
 => if beq_name x x'
       then some T
       else rest x'.

Hint Unfold extend.


(* An extended contexted contains the member we extended it with. 
 *)
Lemma extend_eq 
 : forall A (ctx: partial_map A) x T
 , (extend ctx x T) x = some T.
Proof.
 intros. unfold extend. 
 rewrite <- beq_name_refl. trivial.
Qed.

Hint Resolve extend_eq.


(* We can skip over elements in a context that don't match 
   then one we're looking for.
 *)
Lemma extend_neq
 :  forall A (ctx: partial_map A) x1 T x2
 ,  x2 <> x1
 -> (extend ctx x2 T) x1 = ctx x1.
Proof. 
 intros. unfold extend.
 remember (beq_name x2 x1) as e. destruct e.
 apply true_name_eq in Heqe. subst. contradict H. trivial.
 trivial.
Qed.

Hint Resolve extend_neq.


(* If two elements in a context are bound to different names, 
   then we can swap their order. 
 *)
Lemma extend_swap
 :   forall A (ctx: partial_map A) x T1 y T2
 ,   x <> y
 ->  (extend (extend ctx x T1) y T2) = (extend (extend ctx y T2) x T1).
Proof. 
 intros.
 apply functional_extensionality. intro.
 unfold extend.
 remember (beq_name y x0) as e1.
 remember (beq_name x x0) as e2.
 destruct e1. destruct e2.
  apply true_name_eq in Heqe1.
  apply true_name_eq in Heqe2. subst. contradict H. trivial.
  trivial. trivial.
Qed.
 
Hint Resolve extend_swap.

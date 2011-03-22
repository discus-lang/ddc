(* typing contexts *)
Require Import Base.
Require Import Name.


Definition partial_map (A:Type) 
 := name -> option A.


(* An empty typing context *)
Definition empty {A:Type} : partial_map A 
 := fun _ => none.


(* Extend a typing context *)
Definition extend {A:Type} (rest:partial_map A) (x:name) (T:A)
 := fun x' 
 => if beq_name x x'
       then some T
       else rest x'.


(* An extended contexted contains the member we extended it with *)
Lemma extend_eq 
 : forall A (ctx: partial_map A) x T
 , (extend ctx x T) x = some T.
Proof.
 intros. unfold extend. 
 rewrite <- beq_name_refl. tauto.
Qed.


(* Skippin over members that don't match the name we care about *)
Lemma extend_neq
 :  forall A (ctx: partial_map A) x1 T x2
 ,  beq_name x1 x2 = false
 -> (extend ctx x2 T) x1 = ctx x1.
Proof. 
 intros. unfold extend. 
 rewrite beq_name_sym in H. rewrite -> H. tauto.
Qed.









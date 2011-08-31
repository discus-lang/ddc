
Require Import DDC.Base.

(********************************************************************)
(** * Evaluation Contexts of lists *)
(*  A context defined by one place in a list of exps, where all 
    the exps to the left of it in the list are values:
         v0 v1 v2 xx x4 x5 x6
                  ^^
    This is useful when enforcing a left-to-right evaluation
    order for a list of exps, like in the arguments of an XCon *)
Inductive exps_ctx 
      {exp :Type}         (* type of expressions *)
      (Val: exp -> Prop)  (* predicate to determine whether the exp a value *)
   :  (exp -> list exp)   (* fn to fill the hole in the list evaluation context *)
   -> Prop :=
 | XscHead
   :  forall xs
   ,  exps_ctx Val (fun xx => xx :: xs)

 | XscCons
   :  forall v C
   ,  Val v
   -> exps_ctx Val C
   -> exps_ctx Val (fun xx => v :: C xx).

Hint Constructors exps_ctx.


(* Swapping related expresions in contexts *)
Lemma exps_ctx_Forall2_swap
 :  forall {exp: Type} {B:   Type} 
           (Val:  exp -> Prop)
           (R:    exp -> B -> Prop)
           (C:    exp -> list exp)
           (x x': exp)
           (ys:   list B)
 ,  exps_ctx Val C
 -> (forall y, R x y -> R x' y)
 -> Forall2 R (C x)  ys
 -> Forall2 R (C x') ys.
Proof.
 intros. gen ys.
 induction H; intros.
  destruct ys.
   inverts H1.
   eapply Forall2_cons. eapply H0.
    inverts H1. auto.
    inverts H1. auto.

  inverts H2.
   eapply Forall2_cons.
   auto. auto.
Qed.


(* If all the elements in a list context have a property then
   then they also have this property individually. *)
Lemma exps_ctx_Forall
 :  forall {exp: Type} 
           (Val: exp -> Prop) 
           (C:   exp -> list exp)
           (P:   exp -> Prop)
           (x:   exp)
 ,  exps_ctx Val C
 -> Forall P (C x)
 -> P x.
Proof.
 intros.
 induction H.
  inverts H0. auto.
  inverts H0. auto.
Qed.


Lemma exps_ctx_Forall2_exists_left
 :  forall {exp: Type} {B: Type} 
           (Val: exp -> Prop) 
           (C:   exp -> list exp)
           (R:   exp -> B -> Prop)
           (x:   exp)
           (ys:  list B)
 ,  exps_ctx Val C
 -> Forall2 R (C x) ys
 -> (exists y, R x y).
Proof.
 intros. gen ys.
 induction H; intros.
  destruct ys.
   inverts H0.
   inverts H0. eauto.
  inverts H1.
  eapply IHexps_ctx. eauto.
Qed.


(* Used when evaluating all the expressions in a list.
   If all the exps in a list are either wnf or have some property, 
   then they're either all wnf 
     or there is a context consisting of a run of wnf expressions
        followed by one with the property.

   For example:
     C = w1 w2 w3 w4 x1 ?? ?? ?? ??

   This is a context consisting of a run of four wnfs, followed
   by an expression x1 with the desired property. The rest may or
   may not be wnfs, but the'll all have the property.
*)
Lemma exps_ctx_run
 :  forall {exp: Type} {B: Type}
           (Val:  exp -> Prop) 
           (P:    exp -> Prop) 
           (xs:   list exp)
 ,  Forall (fun x => Val x \/ P x) xs
 -> Forall Val xs 
 \/ (exists C x', exps_ctx Val C 
               /\ xs = C x'
               /\ P x').
Proof.
 intros.
 induction xs.
  left. auto.
  inverts H.

  inverts H2.
   lets D: IHxs H3. clear IHxs.
   inverts D.
    left. auto.
    right. 
     destruct H0 as [C].
     destruct H0 as [x']. 
      inverts H0. inverts H2.
      lets D2: (@XscCons exp Val) H H1.
      exists (fun xx => a :: C xx).
      exists x'. auto.

    lets D: IHxs H3. clear IHxs.
    inverts D.
     right.
     lets D2: (@XscHead exp Val) xs.
     exists (fun xx => xx :: xs).
     exists a. auto.

    destruct H0 as [C].
    destruct H0 as [x'].
     inverts H0. inverts H2.
     right.
     exists (fun xx => xx :: C x').
     exists a.
     lets D2: (@XscHead exp Val) (C x').
     auto.
Qed.



(********************************************************************)
(* Joint contexts of lists. 
   This is used when we evaluate a list of expressions left to right
   where each expression needs to be a value before we move onto
   the next one. *)
Inductive exps_ctx2 
      {exp: Type}        (* type of expressions *)
      (Val: exp -> Prop) (* predicate to determine whether an exp is a value *)
   :  (exp -> list exp)  (* fn for first  evaluation context *)
   -> (exp -> list exp)  (* fn for second evaluation context *)
   -> Prop :=
 | Xsc2Head 
   :  forall xs ys
   ,  exps_ctx2 Val (fun xx => xx :: xs)  (fun yy => yy :: ys)
 
 | Xsc2Cons
   :  forall v C1 C2
   ,  Val v
   -> exps_ctx2 Val C1 C2
   -> exps_ctx2 Val (fun xx => v :: C1 xx) (fun yy => v :: C2 yy).

Hint Constructors exps_ctx2.


(* Take the left of a joint context *)
Lemma exps_ctx2_left
 : forall exp Val C1 C2
 , @exps_ctx2 exp Val C1 C2 -> @exps_ctx exp Val C1.
Proof.
 intros.
 induction H; auto.
Qed.


(* Take the right of a joint context *)
Lemma exps_ctx2_right
 : forall exp Val C1 C2
 , @exps_ctx2 exp Val C1 C2 -> @exps_ctx exp Val C2.
Proof.
 intros.
 induction H; auto.
Qed.


(* Used when evaluating a list of expressions.
   We take an expression from the first list, evaluate it, and 
   place the result in the second. For this to happen we need
   to find an appropriate joint evaluation context.

   If we can produce a wnf for every expression in the first list, 
   then either all exps are already wnf 
     or we can find a joint context consisting of a run of wnf 
        expressions, followed by an expression that we can evaluate.
 
   For example:
    C1 =  w1 w2 w3 x4 ?? ?? ??
    C2 =  w1 w2 w3 w4 ?? ?? ??

   Here we have such a joint context. The first three values in 
   each are idential and already wnf. We then have x4 and y4,
   where x4 can be evaluated into y4. The rest may or may not
   be wnfs, but the'll still be related.

   v1 v2 v3 v4 XX x6 x7 x8 x9
   v1 v2 v3 v4 XX v6 v7 v8 v

*)
Lemma exps_ctx2_run
 :   forall {exp:   Type}
            (Val:   exp -> Prop)
            (R:     exp -> exp -> Prop) 
            (xs ys: list exp)  
 ,   Forall2 (fun x y => R x y /\ Val y /\ (Val x -> y = x)) xs ys
 ->  Forall Val xs
 \/ (exists C1 C2 x' y'
         ,  R x' y'
         /\ exps_ctx2 Val C1 C2 
         /\ xs = C1 x' 
         /\ ys = C2 y').
Proof.
 intros exp Val R xs ys HR.
 induction HR.
  Case "nil".
   left. auto.

  Case "cons".
   rename l  into xs.
   rename l' into ys.
   inverts H. inverts H1.
   inverts IHHR.
   SCase "xs whnf".
    right.
    exists (fun xx => xx :: xs).
    exists (fun xx => xx :: ys).
    exists x.
    exists y. auto.     

   SCase "xs ctx".
    right.
    destruct H1 as [C1].
    destruct H1 as [C2].
    destruct H1 as [x'].
    destruct H1 as [y'].
    inverts H1. inverts H4. inverts H5.
    
    exists (fun xx => xx :: C1 x').
    exists (fun yy => yy :: C2 y').
    exists x. exists y.
    repeat (split; auto).
Qed.


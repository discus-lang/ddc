
Require Import TyJudge.
Require Export Exp.
Require Export SubstExpExp.


(********************************************************************)
(** * Evaluation Contexts of lists *)
(** An evaluation context defines a function that allows us to
    update a node in the AST for an expression. We use this to
    update nodes during single step evaluation. *)

(*  A context defined by one place in a list of exps, where all 
    the exps to the left of it in the list are values:
         v0 v1 v2 xx x4 x5 x6
                  ^^
    This is useful when enforcing a left-to-right evaluation
    order for a list of exps, like in the arguments of an XCon *)
Inductive exps_ctx : (exp -> list exp) -> Prop :=
 | XscIx 
   :  forall ix xs vs x xs'
   ,  splitAt ix xs = (vs, x :: xs')
   -> Forall  whnfX vs
   -> exps_ctx (fun xx => app vs (xx :: xs')).


Lemma exps_ctx_Forall2 
 :   forall {B: Type} (R: exp -> B -> Prop) 
            (x: exp)  Cs
            (y: B)    (ys: list B)
 ,   exps_ctx Cs
 ->  Forall2 R (Cs x) ys
 ->  (exists y, In y ys /\ R x y).
Proof.
 intros.
 inverts H.
 assert (In x (vs ++ x :: xs')). apply in_splitted.
 lets D: Forall2_exists_left_In H H0.
 destruct D. 
 exists x1. eauto.
Qed.  


Lemma exps_ctx_Forall2_swap
 :   forall {B: Type} (R: exp -> B -> Prop)
            (x1 x2 : exp) Cs
            (y: B)        (ys: list B)
 ,   exps_ctx Cs
 ->  (forall y, R x1 y -> R x2 y)
 ->  Forall2 R (Cs x1) ys
 ->  Forall2 R (Cs x2) ys.
Proof.
 intros.
 inverts H.
 lets HP: (@splitAt_app_cons exp) H2.
 lets D: Forall2_app_inv_l H1.
  destruct D as [ys1].
  destruct H as [ys2].
  inverts H. inverts H5.
  apply Forall2_app.
   auto. clear H1. clear H4.
  destruct ys2.
   inverts H.
   inverts H.
   apply Forall2_cons.
    apply H0. auto.
    auto.
Qed.


(********************************************************************)
(*  Evaluation contexts for expressions.
    This describes a place in the exp AST where the sub-expression
    there is able to take an evaluation step *)
Inductive exp_ctx : (exp -> exp) -> Prop :=

 (* The top level context names the entire expression *)
 | XcTop 
   : exp_ctx  (fun x => x)

 (* Left of an application *)
 | XcApp1
   :  forall x2
   ,  exp_ctx  (fun xx => XApp xx x2)

 (* The right of an application can step only when the left is
    already a value. *)
 | XcApp2 
   :  forall v1
   ,  value v1
   -> exp_ctx  (fun xx => XApp v1 xx)

 (* As the XCon constructor contains a list of sub-expressions, 
    we need an additional exps_ctx context to indicate which one 
    we're talking about. *)
 | XcCon 
   :  forall dc C
   ,  exps_ctx C
   -> exp_ctx  (fun xx => XCon dc (C xx))

 (* We need to reduce the discriminant of a case to a value. *)
 | XcCase
   :  forall alts
   ,  exp_ctx  (fun xx => XCase xx alts).

Hint Constructors exp_ctx. 


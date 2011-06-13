
Require Import TyJudge.
Require Export Exp.
Require Export SubstExpExp.


(********************************************************************)
(** * Evaluation Contexts of lists *)
(*  A context defined by one place in a list of exps, where all 
    the exps to the left of it in the list are values:
         v0 v1 v2 xx x4 x5 x6
                  ^^
    This is useful when enforcing a left-to-right evaluation
    order for a list of exps, like in the arguments of an XCon *)
Inductive exps_ctx : (exp -> list exp) -> Prop :=
 | XscHead
   :  exps_ctx (fun xx => xx :: nil)

 | XscCons
   :  forall v C
   ,  whnfX v
   -> exps_ctx C
   -> exps_ctx (fun xx => v :: C xx).

Hint Constructors exps_ctx.


Lemma context_equiv_exp
 :  forall C1 C2 x1 x2
 ,  exps_ctx C1
 -> exps_ctx C2
 -> C1 x1 = C2 x2
 -> x1 = x2.
Proof.
 intros C1 C2 x1 x2 H1 H2. intros.
 gen C2.
  induction H1; intros.
   inverts H2.
    inverts H. auto.
    inverts H1.
     false. false.
   inverts H2.
    inverts H0.
    inverts H1. false. false.
   inverts H0.
   eauto.
Qed.


Lemma context_equiv_ctx
 :  forall C1 C2 x
 ,  exps_ctx C1
 -> exps_ctx C2
 -> C1 x = C2 x
 -> C1 = C2.
Proof.
 intros C1 C2 x H1 H2. intros.
 gen C2.
  induction H1; intros.
   inverts H2.
    auto. inverts H.
    destruct H1.
     false. false.
   inverts H2.
    inverts H0.
     destruct H1.
      false. false.
     inverts H0.
   lets D: IHexps_ctx H4 H6. rewrite D. auto.
Qed.


Lemma context_equiv
 :  forall C1 C2 x1 x2
 ,  exps_ctx C1 
 -> exps_ctx C2
 -> C1 x1 = C2 x2
 -> C1 = C2 /\ x1 = x2.
Proof.
 intros.
 lets D1: context_equiv_exp H H0 H1.
  subst.
 lets D2: context_equiv_ctx H H0 H1.
  subst.
 auto.
Qed.


Lemma context_Forall2_swap
 :  forall {B: Type} (R: exp -> B -> Prop) C
           (x x': exp)
           (ys:   list B)
 ,  exps_ctx C
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


Lemma context_Forall2_exists_left
 :  forall {B: Type} (R: exp -> B -> Prop) C
           (x:  exp)
           (ys: list B)
 ,  exps_ctx C
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


Lemma exps_ctx_Forall
 : forall (P: exp -> Prop) C
          (x: exp)
  ,  exps_ctx C
  -> Forall P (C x)
  -> P x.
Proof.
 intros.
 induction H.
  inverts H0. auto.
  inverts H0. auto.
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


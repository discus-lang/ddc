
Require Import Exp.
Require Import Ty.
Require Import TyJudge.
Require Import KiJudge.
Require Import Env.
Require Import Base.
Require Import SubstTypeType.
Require Import SubstValueValue.

(* Lift type indices that are at least a certain depth. *)
Fixpoint liftTX (n: nat) (depth: nat) (xx: exp) : exp :=
 match xx with
 | XVar _     => xx
 | XLAM x     => XLAM (liftTX n (S depth) x)
 | XAPP x t   => XAPP (liftTX n depth x)  (liftTT n depth t)
 
 | XLam t x   => XLam (liftTT n depth t)  (liftTX n depth x)
 | XApp x1 x2 => XApp (liftTX n depth x1) (liftTX n depth x2)
 end.


(* Substitution of Types in Exps *)
Fixpoint substTX' (depth: nat) (u: ty) (xx: exp) : exp :=
 match xx with
 | XVar _     => xx
 | XLAM x     => XLAM (substTX' (S depth) u x)
 | XAPP x t   => XAPP (substTX' depth u x)  (substTT' depth u t)

 | XLam t x   => XLam (substTT' depth u t)  (substTX' depth u x)
 | XApp x1 x2 => XApp (substTX' depth u x1) (substTX' depth u x2)
 end.


Definition  substTX := substTX' 0.
Hint Unfold substTX.


(* Lifting Lemmas ***************************************************)

(* Lifting a type index by zero steps doesn't do anything. *)
Theorem liftTX_zero
 :  forall x depth
 ,  liftTX 0 depth x = x.
Proof.
 induction x; intros; simpl;
  try (auto);
  try (rewrite IHx;  auto);
  try (rewrite IHx1; auto; rewrite IHx2; auto);
  try (rewrite liftTT_zero; auto).
Qed.


(* Lifting covered indices doesn't do anything. *)
Theorem liftTX_covers
 :  forall ix n t
 ,  coversTX n t
 -> liftTX ix n t = t.
Proof.
 intros ix n t.
 gen n.
 induction t; intros; inverts H; simpl;
  try auto;
  try (rewrite IHt; auto);
  try (rewrite IHt1; auto; rewrite IHt2; auto);
  try (rewrite liftTT_covers; auto).
Qed.



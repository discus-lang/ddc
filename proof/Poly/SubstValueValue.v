
Require Import Exp.
Require Import Base.


(* Lift value indices that are at least a certain depth. *)
Fixpoint liftXX (n: nat) (depth: nat) (xx: exp) : exp :=
 match xx with
 | XVar ix    => if bge_nat ix depth
                  then XVar (ix + n)
                  else xx

 | XLAM x     => XLAM (liftXX n depth x)
 | XAPP x t   => XAPP (liftXX n depth x) t
 
 | XLam t x   => XLam t (liftXX n (S depth) x)
 | XApp x1 x2 => XApp (liftXX n depth x1)
                      (liftXX n depth x2)
 end.


(* Substitution of Exps in Exps *)
Fixpoint substXX' (depth: nat) (u: exp) (xx: exp) : exp :=
 match xx with
 | XVar ix    => match compare ix depth with
                 | EQ => liftXX depth 0 u
                 | GT => XVar (ix - 1)
                 | _  => XVar  ix
                 end

 | XLAM x     => XLAM (substXX' depth u x)
 | XAPP x t   => XAPP (substXX' depth u x) t

 | XLam t x   => XLam t (substXX' (S depth) u x)
 | XApp x1 x2 => XApp (substXX' depth u x1)
                      (substXX' depth u x2)
 end.


Definition  substXX := substXX' 0.
Hint Unfold substXX.


(* Lifting Lemmas ***************************************************)

(* Lifting an type by zero steps doesn't do anything. *)
Theorem liftXX_zero
 :  forall x depth
 ,  liftXX 0 depth x = x.
Proof.
 induction x; intros; simpl; 
  try (rewrite IHx; auto).

 Case "TVar".
  breaka (bge_nat n depth).

 Case "TApp".
  rewrite IHx1. rewrite IHx2. auto.
Qed.


(* Lifting covered indices doesn't do anything. *)
Theorem liftXX_covers
 :  forall ix n t
 ,  coversXX n t
 -> liftXX ix n t = t.
Proof.
 intros ix n t.
 gen n.
 induction t; intros; inversions H; simpl;
  try (rewrite IHt; auto).

 Case "XVar".
  break (bge_nat n n0).
  apply bge_nat_true in HeqX.
  false. omega.
  auto.

 Case "XApp".
  rewrite IHt1. rewrite IHt2. auto. auto. auto.
Qed.


(* If a type is closed, then lifting it doesn't do anything. *)
Theorem liftXX_closed
 :  forall ix x
 ,  closedXX x 
 -> liftXX ix 0 x = x. 
Proof.
 intros.
 apply liftXX_covers. inversions H. auto.
Qed.



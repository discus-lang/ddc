
Require Import Exp.
Require Import Ty.
Require Import TyJudge.
Require Import KiJudge.
Require Import Env.
Require Import Base.
Require Import SubstTypeType.


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


(* Theorems *********************************************************)


(* Substitution of values in values preserves typing. *)
Theorem subst_value_value_drop
 :  forall ix kenv tenv x1 t1 x2 t2
 ,  closedXX x2
 -> closedXT x2
 -> get  tenv ix = Some t2
 -> TYPE kenv tenv           x1 t1
 -> TYPE kenv (drop ix tenv) x2 t2
 -> TYPE kenv (drop ix tenv) (substXX' ix x2 x1) t1.
Proof.
 intros. gen ix kenv tenv t1 t2.
 induction x1; intros; inversions H2; simpl; eauto.

 Case "XVar". 
  fbreak_compare.
  SCase "n = ix".
   rewrite liftXX_closed; auto.
   rewrite H1 in H7. inversions H7. auto.

  SCase "n < ix".
   apply TYVar. rewrite <- H7. auto.

  SCase "n > ix".
   apply TYVar. rewrite <- H7.
   destruct n.
    false. omega.
    simpl. rewrite nat_minus_zero. apply get_drop_below. omega.

 Case "XLAM".
  eapply TYLAM. eapply IHx1; eauto.
  eapply type_check_tyclosed_in_any_tyenv; auto.
  eapply type_check_kiclosed_in_any_kienv; auto.
  eauto.

 Case "XLam".
  apply TYLam. rewrite drop_rewind.
  eapply IHx1; eauto.
  simpl. eapply type_check_tyclosed_in_any_tyenv. auto. eauto.
Qed.


Theorem subst_value_value
 :  forall kenv tenv x1 t1 x2 t2
 ,  closedXX x2
 -> closedXT x2
 -> TYPE kenv (tenv :> t2)   x1 t1
 -> TYPE kenv tenv x2 t2
 -> TYPE kenv tenv (substXX x2 x1) t1.
Proof.
 intros.
 assert (tenv = drop 0 (tenv :> t2)). auto. rewrite H3. clear H3.
 unfold substXX.
 eapply subst_value_value_drop; simpl; eauto.
Qed.



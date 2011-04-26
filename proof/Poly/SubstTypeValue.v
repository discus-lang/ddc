
Require Import SubstTypeType.
Require Import TyJudge.
Require Import KiJudge.
Require Import Exp.
Require Import Env.
Require Import Base.


(* Lift type indices that are at least a certain depth. *)
Fixpoint liftTX (n: nat) (d: nat) (xx: exp) : exp :=
  match xx with
  |  XVar _     => xx

  |  XLAM x     
  => XLAM (liftTX n (S d) x)

  |  XAPP x t 
  => XAPP (liftTX n d x)  (liftTT n d t)
 
  |  XLam t x   
  => XLam (liftTT n d t)  (liftTX n d x)

  |  XApp x1 x2
  => XApp (liftTX n d x1) (liftTX n d x2)
 end.


(* Substitution of Types in Exps *)
Fixpoint substTX' (d: nat) (u: ty) (xx: exp) : exp :=
  match xx with
  | XVar _     => xx

  |  XLAM x     
  => XLAM (substTX' (S d) (liftTT 1 0 u) x)

  |  XAPP x t
  => XAPP (substTX' d u x)  (substTT' d u t)

  |  XLam t x
  => XLam (substTT' d u t)  (substTX' d u x)

  |  XApp x1 x2
  => XApp (substTX' d u x1) (substTX' d u x2)
 end.


Definition  substTX := substTX' 0.
Hint Unfold substTX.


Lemma substTT_liftTT
 :  forall d t1 t2
 ,  substTT' d t2 (liftTT 1 d t1) = t1.
Proof.
 intros. gen d t2.
 induction t1; intros.
 simpl; auto.
 simpl. break (bge_nat n d). admit.
 simpl. admit.

 Case "TForall".
  simpl. rewrite IHt1. auto.

 Case "TFun".
  simpl. rewrite IHt1_1. rewrite IHt1_2. auto.
Qed.

  


Theorem subst_type_value_drop
 :  forall ix ke te x1 t1 t2 k2
 ,  get ke ix = Some k2
 -> TYPE ke           te x1 t1
 -> KIND (drop ix ke)    t2 k2
 -> TYPE (drop ix ke) 
         (substTE  t2 te)
         (substTX' ix t2 x1)
         (substTT  t2 t1).
Proof.
 intros. gen ix ke te t1 t2 k2.
 induction x1; intros; simpl; inverts H0; eauto.

 Case "XVar".
  apply TYVar. admit. (* ok, lemma about map *)

 Case "XLAM".
  assert (substTT t2 (TForall t0) = TForall (substTT' 1 (liftTT 1 0 t2) t0)). auto.
  rewrite H0. clear H0.
  apply TYLAM. rewrite drop_rewind.
  assert (substTT' 1 (liftTT 1 0 t2) t0 = substTT (liftTT 1 0 t2) t0). admit. rewrite H0.
  assert (liftTE 1 (substTE t2 te)     = substTE (liftTT 1 0 t2) (liftTE 1 te)).
  admit. rewrite H2.
  eapply IHx1. auto. simpl. eauto. simpl. apply liftTT_push. auto.

  apply IHx1.






























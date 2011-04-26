
Require Import SubstTypeType.
Require Import TyJudge.
Require Import KiJudge.
Require Import Exp.
Require Import Env.
Require Import Base.


(* Lift type indices that are at least a certain depth. *)
Fixpoint liftTX' (d: nat) (xx: exp) : exp :=
  match xx with
  |  XVar _     => xx

  |  XLAM x     
  => XLAM (liftTX' (S d) x)

  |  XAPP x t 
  => XAPP (liftTX' d x)  (liftTT' d t)
 
  |  XLam t x   
  => XLam (liftTT' d t)  (liftTX' d x)

  |  XApp x1 x2
  => XApp (liftTX' d x1) (liftTX' d x2)
 end.


(* Substitution of Types in Exps *)
Fixpoint substTX' (d: nat) (u: ty) (xx: exp) : exp :=
  match xx with
  | XVar _     => xx

  |  XLAM x     
  => XLAM (substTX' (S d) (liftTT u) x)

  |  XAPP x t
  => XAPP (substTX' d u x)  (substTT' d u t)

  |  XLam t x
  => XLam (substTT' d u t)  (substTX' d u x)

  |  XApp x1 x2
  => XApp (substTX' d u x1) (substTX' d u x2)
 end.


Definition  substTX := substTX' 0.
Hint Unfold substTX.


(* For two types t1, t2. 
   If we lift t1 by d steps, then substitute t2 into it at the same depth 
    then 1) substitution never takes place, because the free indices are now all >= d.
         2) the substitution process subtracts d from each free index
    so we get the same t1 we had when we started.
 *)
Lemma substTT_liftTT
 :  forall d t1 t2
 ,  substTT' d t2 (liftTT' d t1) = t1.
Proof.
 intros. gen d t2.
 induction t1; intros; eauto.

 Case "TVar".
  simpl. breaka (bge_nat n d).
  SCase "n >= d".
   destruct d.
    simpl. nnat. auto.
    simpl. breaka (compare n d).
     false.
     apply compare_eq in HeqX0. subst.
     admit. (* ok lemma on ordering *)
     admit. (* ok lemma on ordering *)
     nnat. auto.

  admit. (* ok, less than *)

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
  assert (substTT t2 (TForall t0) = TForall (substTT' 1 (liftTT t2) t0)). auto.
  rewrite H0. clear H0.
  apply TYLAM. rewrite drop_rewind.
  unfold liftTT.

  assert (substTT' 1 (liftTT' 0 t2) t0 = substTT (liftTT' 0 t2) t0). admit. rewrite H0.
  assert (liftTE (substTE t2 te)     = substTE (liftTT t2) (liftTE te)).
  admit. rewrite H2.
  eapply IHx1. auto. simpl. eauto. simpl. apply liftTT_push. auto.

  apply IHx1.






























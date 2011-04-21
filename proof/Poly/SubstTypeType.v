
Require Import KiJudge.
Require Import Base.

(* Lift type indices that are at least a certain depth. *)
Fixpoint liftT (n: nat) (depth: nat) (tt: ty) : ty :=
 match tt with
 | TCon _     => tt

 | TVar it    => if bge_nat it depth
                  then TVar (it + n)
                  else tt

 | TForall t  => TForall (liftT n (S depth) t)
 | TFun t1 t2 => TFun (liftT n depth t1)
                      (liftT n depth t2)
 end.


(* Substitution of Types in Types *)
Fixpoint substT' (depth: nat) (u: ty) (tt: ty) : ty :=
 match tt with
 | TCon _     => tt
 
 | TVar it    => match compare it depth with
                 | EQ => liftT depth 0 u
                 | GT => TVar (it - 1)
                 | _  => TVar  it
                 end

 | TForall t  => TForall (substT' (S depth) u t)
 | TFun t1 t2 => TFun (substT' depth u t1)
                      (substT' depth u t2)
 end.


Definition  substT := substT' 0.
Hint Unfold substT.


(* Lifting Lemmas ***************************************************)

(* Lifting an type by zero steps doesn't do anything. *)
Theorem liftT_zero
 :  forall t1 depth
 ,  liftT 0 depth t1 = t1.
Proof.
 induction t1; intros; simpl; 
  try (auto; rewrite IHt1; auto).

 Case "TVar".
  breaka (bge_nat n depth).

 Case "TFun".
  rewrite IHt1_1. rewrite IHt1_2. auto.
Qed.


(* Lifting covered indices doesn't do anything. *)
Theorem liftT_covers
 :  forall it n t
 ,  coversT n t
 -> liftT it n t = t.
Proof.
 intros it n t. gen n.
 induction t; intros; inversions H; simpl.
  try (auto; rewrite IHt; auto).

 Case "TVar".
  break (bge_nat n n0).
  apply bge_nat_true in HeqX.
  false. omega.
  auto.

 Case "TForall".
  rewrite IHt; auto.

 Case "TFun".
  rewrite IHt1. rewrite IHt2. auto. auto. auto.
Qed.


(* If a type is closed, then lifting it doesn't do anything. *)
Theorem liftT_closed
 :  forall it t
 ,  closedT t 
 -> liftT it 0 t = t. 
Proof.
 intros.
 apply liftT_covers. inversions H. auto.
Qed.


(* Theorems *********************************************************)

(* Substitution of types in types preserves kinding. *)
Theorem subst_type_type_drop
 :  forall it kenv t1 k1 t2 k2
 ,  closedT t2
 -> get  kenv it = Some k2
 -> KIND kenv           t1 k1
 -> KIND (drop it kenv) t2 k2
 -> KIND (drop it kenv) (substT' it t2 t1) k1.
Proof.
 intros it kenv t1 k1 t2 k2.
 gen it kenv k1.
 induction t1; intros; simpl.

 Case "TCon".
  destruct k1. auto.

 Case "TVar".
  break (compare n it).
  SCase "n = it".
   apply compare_eq in HeqX. subst.
   rewrite liftT_closed; auto.
   inversions H1. rewrite H0 in H5.
   inversion H5. destruct k1. destruct k2.
   auto.

  SCase "n < it".
   apply compare_lt in HeqX.
   apply KIVar. inversions H1. rewrite <- H5.
   apply get_drop_above. auto.

  SCase "n > it".
   apply compare_gt in HeqX.
   apply KIVar. inversions H1. rewrite <- H5.
   destruct n.
    simpl. false. omega.
    simpl. rewrite nat_minus_zero. apply get_drop_below. omega.

 Case "TForall".
  destruct k1. inversions H1.
  apply KIForall. rewrite drop_rewind. apply IHt1; eauto.
  simpl. eapply kind_check_closed_in_any; eauto.

 Case "TFun".
  destruct k1. inversions H1.
  apply KIFun; eauto.
Qed.


Theorem subst_type_type
 :  forall kenv t1 k1 t2 k2
 ,  closedT t2
 -> KIND (kenv :> k2) t1 k1
 -> KIND kenv         t2 k2
 -> KIND kenv (substT t2 t1) k1.
Proof.
 intros.
 assert (kenv = drop 0 (kenv :> k2)). auto. rewrite H2. clear H2.
 unfold substT.
 eapply subst_type_type_drop; simpl; eauto.
Qed.




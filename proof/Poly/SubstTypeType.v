
Require Import Exp.
Require Import TyEnv.
Require Import WellFormed.
Require Import KiJudge.
Require Import Base.


(* Lift type indices that are at least a certain depth. *)
Fixpoint liftTT (n: nat) (depth: nat) (tt: ty) : ty :=
 match tt with
 | TCon _     => tt

 | TVar it    => if bge_nat it depth
                  then TVar (it + n)
                  else tt

 | TForall t  => TForall (liftTT n (S depth) t)
 | TFun t1 t2 => TFun    (liftTT n depth t1)
                         (liftTT n depth t2)
 end.


(* Substitution of Types in Types *)
Fixpoint substTT' (depth: nat) (u: ty) (tt: ty) : ty :=
 match tt with
 | TCon _     => tt
 
 | TVar it    => match compare it depth with
                 | EQ => liftTT depth 0 u
                 | GT => TVar (it - 1)
                 | _  => TVar  it
                 end

 | TForall t  => TForall (substTT' (S depth) u t)
 | TFun t1 t2 => TFun    (substTT' depth u t1)
                         (substTT' depth u t2)
 end.


Definition  substTT := substTT' 0.
Hint Unfold substTT.


(* Lifting Lemmas ***************************************************)

(* Lifting an type by zero steps doesn't do anything. *)
Theorem liftTT_zero
 :  forall t1 depth
 ,  liftTT 0 depth t1 = t1.
Proof.
 induction t1; intros; simpl; 
  try auto;
  try (rewrite IHt1; auto).

 Case "TVar".
  breaka (bge_nat n depth).

 Case "TFun".
  rewrite IHt1_1. rewrite IHt1_2. auto.
Qed.


Theorem liftT_wfT
 :  forall ix n t e
 ,  n = length e
 -> wfT e t 
 -> liftTT ix n t = t.
Proof.
 intros. gen e n.
 induction t; intros; simpl; simpl in H; eauto.

 Case "TVar".
  breaka (bge_nat n n0).
  apply bge_nat_true in HeqX.
  false. subst. destruct H0.
  eapply getMatch_above_false; eauto.

 Case "TForall".
  eapply IHt in H0; eauto.
  simpl in H0. symmetry. rewrite H. rewrite H0. auto.

 Case "TFun".
  destruct H0.
  lets D1: IHt1 H0 H. rewrite D1.
  lets D2: IHt2 H1 H. rewrite D2.
  auto.
Qed.


(* If a type is closed, then lifting it doesn't do anything. *)
Theorem liftTT_closed
 :  forall it t
 ,  closedT t 
 -> liftTT it 0 t = t. 
Proof.
 intros. unfold closedT in H. eapply liftT_wfT; eauto.
 auto.
Qed.


(* Theorems *********************************************************)

(* Substitution of types in types preserves kinding. *)
Theorem subst_type_type_drop
 :  forall it kenv t1 k1 t2 k2
 ,  closedT t2
 -> get  kenv it = Some k2
 -> KIND kenv           t1 k1
 -> KIND (drop it kenv) t2 k2
 -> KIND (drop it kenv) (substTT' it t2 t1) k1.
Proof.
 intros it kenv t1 k1 t2 k2.
 gen it kenv k1.
 induction t1; intros; simpl; inverts H1; eauto.

 Case "TVar".
  destruct k1. destruct k2.
  fbreak_compare.
  SCase "n = it".
   rewrite liftTT_closed; auto.

  SCase "n < it".
   apply KIVar. rewrite <- H5. auto.

  SCase "n > it".
   apply KIVar. rewrite <- H5.
   destruct n.
    simpl. false. omega.
    simpl. rewrite nat_minus_zero. apply get_drop_below. omega.

 Case "TForall".
  apply KIForall. rewrite drop_rewind.
  apply IHt1; eauto. simpl.
  eapply kind_check_closed_in_any; eauto.
Qed.


Theorem subst_type_type
 :  forall kenv t1 k1 t2 k2
 ,  closedT t2
 -> KIND (kenv :> k2)  t1 k1
 -> KIND kenv          t2 k2
 -> KIND kenv (substTT t2 t1) k1.
Proof.
 intros.
 assert (kenv = drop 0 (kenv :> k2)). auto. rewrite H2. clear H2.
 unfold substTT.
 eapply subst_type_type_drop; simpl; eauto.
Qed.



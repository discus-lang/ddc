
Require Import TyJudge.
Require Export Exp.


(** Substitution ******************************************)
Fixpoint liftX (n: nat) (depth: nat) (tt: exp) : exp :=
 match tt with 
 | XVar ix    => if bge_nat ix depth
                  then XVar (ix + n)
                  else tt

 | XLam T1 t1 => XLam T1 (liftX n (S depth) t1)

 | XApp t1 t2 => XApp (liftX n depth t1)
                      (liftX n depth t2)
 end.


Fixpoint subst' (depth: nat) (u: exp) (tt: exp)  : exp :=
 match tt with
 | XVar ix    =>  match compare ix depth with
                  | EQ => liftX depth 0 u
                  | GT => XVar (ix - 1)
                  | _  => XVar ix
                  end

 | XLam T1 t2 => XLam T1 (subst' (S depth) u t2)

 | XApp t1 t2 => XApp (subst' depth u t1)
                      (subst' depth u t2)
 end. 

Definition  subst := subst' 0.
Hint Unfold subst.


(** Lemmas **********************************************************)

(* Lifting an expression by 0 steps doesn't do anything *)
Theorem liftX_none
 : forall t1 depth
 , liftX 0 depth t1 = t1.
Proof.
 induction t1; intro; simpl.

 Case "XVar".
  assert (n + 0 = n). omega. rewrite H.
  breaka (bge_nat n depth).

 Case "XLam".
  rewrite IHt1. auto.

 Case "XApp". 
  rewrite IHt1_1. rewrite IHt1_2. auto.
Qed.


Theorem liftX_wfX
 :  forall ix n x tenv
 ,  n = length tenv
 -> wfX tenv x -> liftX ix n x = x.
Proof.
 intros. gen tenv n.
 induction x; intros; simpl; simpl in H0.
 
 Case "XVar".
  breaka (bge_nat n n0).
  apply bge_nat_true in HeqX.
  false. subst. destruct H0.
  eapply get_above_false in H; auto.

 Case "XLam".
  eapply IHx in H0; eauto.
  simpl in H0. symmetry. rewrite H. rewrite H0. auto.

 Case "XApp".
  destruct H0.
  lets D1: IHx1 H0 H. rewrite D1.
  lets D2: IHx2 H1 H. rewrite D2.
  auto.
Qed.


(* If a term is closed then lifting it doesn't do anything *)
Theorem liftX_closed
 : forall ix t
 , closedX t -> liftX ix 0 t = t.
Proof.
 intros. unfold closedX in H. eapply liftX_wfX; eauto. 
 simpl. auto.
Qed.


Theorem subst_value_value_drop
 :  forall ix tenv t1 t2 T1 T2
 ,  get tenv ix = Some T2
 -> closedX t2
 -> TYPE tenv           t1 T1
 -> TYPE (drop ix tenv) t2 T2
 -> TYPE (drop ix tenv) (subst' ix t2 t1) T1.
Proof.
 intros ix tenv t1 t2 T1 T2. gen ix tenv T1.
 induction t1; intros; simpl; inverts H1.

 Case "XVar".
  fbreak_compare.
  SCase "n = ix".
   rewrite liftX_closed; eauto.
   rewrite H in H5. inverts H5. auto.

  SCase "n < ix".
   apply TYVar. rewrite <- H5. auto.
   
  SCase "n > ix".
   apply TYVar. rewrite <- H5.
   destruct n. 
    false. omega.
    simpl. rewrite nat_minus_zero. apply get_drop_below. omega.

 Case "XLam".
  apply TYLam. rewrite drop_rewind.
  apply IHt1; auto.
  eapply type_check_closed_in_any_tyenv; eauto.

 Case "XApp".
  eauto.
Qed.


Theorem subst_value_value
 :  forall tenv t1 t2 T1 T2
 ,  closedX t2
 -> TYPE (tenv :> T2) t1 T1
 -> TYPE tenv         t2 T2 
 -> TYPE tenv (subst t2 t1) T1.
Proof. 
 intros tenv t1 t2 T1 T2 Ht1 Ht2.
 lets H: subst_value_value_drop 0 (tenv :> T2).
  simpl in H. eapply H; eauto.
Qed.




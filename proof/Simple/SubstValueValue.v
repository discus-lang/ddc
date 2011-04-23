
Require Import TyJudge.
Require Export Exp.


(** Substitution ****************************************************)
Fixpoint liftX (n: nat) (depth: nat) (xx: exp) : exp :=
 match xx with 
 | XVar ix    => if bge_nat ix depth
                  then XVar (ix + n)
                  else xx

 | XLam t1 x1 => XLam t1 (liftX n (S depth) x1)

 | XApp x1 x2 => XApp (liftX n depth x1)
                      (liftX n depth x2)
 end.


Fixpoint subst' (depth: nat) (u: exp) (xx: exp)  : exp :=
 match xx with
 | XVar ix    =>  match compare ix depth with
                  | EQ => liftX depth 0 u
                  | GT => XVar (ix - 1)
                  | _  => XVar ix
                  end

 | XLam t1 x2 => XLam t1 (subst' (S depth) u x2)

 | XApp x1 x2 => XApp (subst' depth u x1)
                      (subst' depth u x2)
 end. 

Definition  subst := subst' 0.
Hint Unfold subst.


(** Lemmas **********************************************************)

(* Lifting an expression by 0 steps doesn't do anything *)
Theorem liftX_none
 : forall x1 depth
 , liftX 0 depth x1 = x1.
Proof.
 induction x1; intro; simpl.

 Case "XVar".
  assert (n + 0 = n). omega. rewrite H.
  breaka (bge_nat n depth).

 Case "XLam".
  rewrite IHx1. auto.

 Case "XApp". 
  rewrite IHx1_1. rewrite IHx1_2. auto.
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
 : forall ix x
 , closedX x -> liftX ix 0 x = x.
Proof.
 intros. unfold closedX in H. eapply liftX_wfX; eauto. 
 simpl. auto.
Qed.


Theorem subst_value_value_drop
 :  forall ix tenv x1 x2 t1 t2
 ,  get tenv ix = Some t2
 -> closedX x2
 -> TYPE tenv           x1 t1
 -> TYPE (drop ix tenv) x2 t2
 -> TYPE (drop ix tenv) (subst' ix x2 x1) t1.
Proof.
 intros ix tenv x1 x2 t1 t2. gen ix tenv t1.
 induction x1; intros; simpl; inverts H1.

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
  apply IHx1; auto.
  eapply type_check_closed_in_any_tyenv; eauto.

 Case "XApp".
  eauto.
Qed.


Theorem subst_value_value
 :  forall tenv x1 x2 t1 t2
 ,  closedX x2
 -> TYPE (tenv :> t2) x1 t1
 -> TYPE tenv         x2 t2 
 -> TYPE tenv (subst x2 x1) t1.
Proof. 
 intros tenv x1 x2 t1 t2 Ht1 Ht2.
 lets H: subst_value_value_drop 0 (tenv :> t2).
  simpl in H. eapply H; eauto.
Qed.


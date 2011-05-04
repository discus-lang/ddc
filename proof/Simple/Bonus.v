(* Bonus lemmas that aren't used by the main proofs *)

Require Import Base.
Require Import Env.
Require Import Exp.
Require Import TyJudge.
Require Import SubstExpExp.


(* Weakening type environments. *************************************)
Theorem type_tyenv_weaken1
 :  forall tenv x t1 t2
 ,  TYPE tenv          x t1
 -> TYPE (t2 <: tenv)  x t1.
Proof.
 intros. gen tenv t1.
 induction x; intros; inversions H; eauto.

 Case "XLam".
  eapply TYLam. rewrite snoc_cons. auto.
Qed.


Theorem type_tyenv_weaken
 :  forall tenv1 tenv2 x1 t1
 ,  TYPE tenv1            x1 t1
 -> TYPE (tenv2 ++ tenv1) x1 t1.
Proof.
 intros. gen tenv1.
 induction tenv2; intros.
  rewrite append_empty_left. auto.
  rewrite append_snoc.  apply IHtenv2.
   apply type_tyenv_weaken1. auto.
Qed.


(* Strengthen type environments *************************************)
Theorem type_tyenv_strengthen
 :  forall tenv tenv' n x t
 ,   wfX tenv' x
 ->  tenv' = take n tenv
 ->  TYPE tenv  x t
 ->  TYPE tenv' x t.
Proof.
 intros. gen tenv tenv' n t.
 induction x; intros; inversions H1.
 
 Case "XVar".
  apply TYVar.
   destruct H. eauto. 

 Case "XLam".
  simpl in H.
  eapply TYLam.
   eapply IHx with (n := S n) (tenv := tenv :> t); auto.

 Case "XApp".
  simpl in H. destruct H. eauto.
Qed.


Theorem type_check_closed_in_empty_tyenv
 :  forall tenv x t
 ,  closedX x
 -> TYPE tenv  x t
 -> TYPE Empty x t.
Proof.
 intros. unfold closedX in H. 
 eapply type_tyenv_strengthen; eauto.
 eapply take_zero.
Qed.


Theorem type_check_closed_in_any_tyenv
 :  forall tenv tenv' x1 t1
 ,  closedX x1
 -> TYPE tenv  x1 t1
 -> TYPE tenv' x1 t1.
Proof.
 intros.
 lets D: type_check_closed_in_empty_tyenv H H0.
 assert (TYPE (tenv' ++ Empty) x1 t1).
  apply type_tyenv_weaken. auto.
  auto.
Qed.


(* Lifting an expression by 0 steps doesn't do anything.
   This is equivalent to pushing 0 things on the environment. *)
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


(* If an expression is well formed under a given environment, 
   then all its indices are less than the length of this environment. 
   Lifting indices more than this length doesn't do anything *)
Theorem liftX_wfX
 :  forall n d x e
 ,  d = length e
 -> wfX e x
 -> liftX n d x = x.
Proof.
 intros. gen e d.
 induction x; intros; simpl; simpl in H0.
 
 Case "XVar".
  breaka (bge_nat n0 d).
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


(* If an expression is closed then it has no free indices. 
   Lifting it doesn't do anything. *)
Lemma liftX_closed
 :  forall n x
 ,  closedX x
 -> liftX n 0 x = x.
Proof.
 intros. unfold closedX in H. eapply liftX_wfX; eauto. 
 simpl. auto.
Qed.


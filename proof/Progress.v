
Require Import TyJudge.
Require Import EvJudge.
Require Import FunctionalExtensionality.

Theorem closed_var_not
 : forall n
 , ~(closed (XVar n)).
Proof.
 intro. unfold not. intro.
 unfold closed in H.
 specialize H with n. contradict H. apply FreeX_var.
Qed.


Theorem functional_extensionality_not
 : forall A B (f : A -> B) (g : A -> B)
 , (exists x, f x <> g x) -> f <> g.
Proof.
 intros. unfold not. intro. subst.
 inversion H. contradiction H0.
 trivial.
Qed.


Theorem check_closed_in_empty
 : forall env t T 
 , closed t -> TYPE env t T -> TYPE empty t T.
Proof.
 intros.
 eapply tyenv_invariance.
 apply H0.
 intros. contradict H1. unfold closed in H. apply H.
Qed.


Theorem closed_lam
 : forall x T t 
 , closed t -> closed (XLam x T t).
Proof. 
 intros. unfold closed. intros. unfold not. intro.
 inversion H0. subst.
 unfold closed in H. apply H in H6. assumption.
Qed.


Theorem closed_app
 : forall t1 t2
 , closed t1 -> closed t2 -> closed (XApp t1 t2).
Proof. 
 intros.
 unfold closed. intros. unfold not. intro.
 unfold closed in H.  specialize H  with x.
 unfold closed in H0. specialize H0 with x.
 inversion H1. subst. auto. subst. auto.
Qed.


Theorem check_empty_is_closed
 : forall t T
 , TYPE empty t T -> closed t.
Proof.
 intros t T.
 generalize T.
 induction t.
 Case "XVar".
  intros. inversion H. subst. unfold empty in H2. inversion H2.
 Case "XLam".
  intros. inversion H. subst.
  unfold closed. intro.
  unfold not. intro.
  eapply tyenv_contains_free_vars with (x:=x) in H.
  inversion H. inversion H1. assumption.
 Case "XApp".
  intros. inversion H. subst.
  apply closed_app.
  eapply IHt1. apply H3.
  eapply IHt2. apply H5.
Qed.


Theorem progress
 :  forall t T
 ,  TYPE  empty t T
 -> VALUE t \/ (exists t', STEP t t').
Proof.
 intros t T. intros Ht.
 remember (@empty ty) as G.
 induction Ht.
 Case "XVar".
  left. subst. inversion H. subst.
 Case "XLam".
  left. apply VALUE_XLam.
  apply TYLam in Ht.
  eapply check_empty_is_closed. apply Ht.
 Case "XApp".
  right.
  destruct IHHt1. assumption. 
  SCase "t1 value".
   destruct H. destruct IHHt2. assumption.
   SSCase "t2 value".
    exists (subst x t2 t). apply EVAppAbs. assumption.
   SSCase "t2 steps".
    destruct H0 as [t2' Htsp].
    exists (XApp (XLam x T t) t2').
    apply EVApp2. apply VALUE_XLam. assumption. assumption.
  SCase "t1 steps".
   destruct H as [t1' Htsp].
   exists (XApp t1' t2).
   apply EVApp1. assumption.
Qed.




Require Import TyJudge.
Require Import EvJudge.


Theorem progress
 :  forall t T
 ,  TYPE  empty t T
 -> VALUE t \/ (exists t', STEP t t').
Proof.
 intros t T. intros Ht.
 remember (@empty ty) as G.
 induction Ht.
 Case "XVar".
  left. subst. inversion H.
 Case "XLam".
  left. auto.
 Case "XApp".
  right.
  destruct IHHt1. auto. 
  SCase "t1 value".
   destruct H. destruct IHHt2. trivial.
   SSCase "t2 value".
    exists (subst x t2 t). apply EVAppAbs. trivial.
   SSCase "t2 steps".
    destruct H as [t2' Htsp].
    exists (XApp (XLam x T t) t2').
    apply EVApp2. trivial. trivial.
  SCase "t1 steps".
   destruct H as [t1' Htsp].
   exists (XApp t1' t2).
   apply EVApp1. trivial.
Qed.



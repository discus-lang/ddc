
Require Import TyJudge.
Require Import EvJudge.


(* A well typed term is either a value, or can transition to the next state. *)
Theorem progress
 :  forall t T
 ,  TYPE  empty t T
 -> value t \/ (exists t', STEP t t').
Proof.
 intros t T. intros Ht.
 remember (@empty ty) as G.
 induction Ht.
 Case "XVar".
  left. subst. inversion H. subst.
 Case "XLam".
  left. apply Value_lam.
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
    apply EVApp2. apply Value_lam. assumption. assumption.
  SCase "t1 steps".
   destruct H as [t1' Htsp].
   exists (XApp t1' t2).
   apply EVApp1. assumption.
Qed.



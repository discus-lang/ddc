
Require Import TyJudge.
Require Import EvJudge.


(* A well typed term is either a value,
     or can transition to the next state. *)
Theorem progress
 :  forall t T
 ,  TYPE  empty empty t T
 -> value t \/ (exists t', STEP t t').
Proof.
 intros t T. intros Ht.
 remember (@empty ki) as Gk.
 remember (@empty ty) as Gt.
 induction Ht.

 Case "XVar".
  left. subst. inversion H0.

 Case "XLam".
  left. apply Value_lam.
  apply TYLam in Ht.
  eapply check_empty_is_closed. subst. eauto. auto.

 Case "XApp".
  right.
  destruct IHHt1; auto.
  SCase "t1 value".
   destruct IHHt2; auto.
   SSCase "t2 value".
    inversions H.
     exists (substXX x t2 t). apply EVAppLam. auto.
     inversion Ht1.
   SSCase "t2 steps".
    destruct H0 as [t2' Htsp].
    inversions H.
     exists (XApp (XLam x T t) t2'). apply EVApp2; auto.
     inversion Ht1.
  SCase "t1 steps".
   destruct H as [t1' Htsp].
   exists (XApp t1' t2). apply EVApp1. auto.

 Case "XLAM".
   left.
   apply Value_LAM. apply TYLAM in Ht.
   eapply check_empty_is_closed. subst. eauto. auto.

 Case "XAPP".
   right.
   destruct IHHt; auto.
   SCase "t1 value".
    inversions H1.
    inversion  Ht.
    exists (substTX x T2 t). apply EVAPPLAM.
    eapply kind_check_empty_is_closed. eauto.

   SCase "t1 steps".
    destruct H1 as [t1' Hstp].
    exists (XAPP t1' T2).
    eapply EVAPP1. auto.
Qed.


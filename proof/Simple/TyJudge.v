
Require Export Exp.


(** Type Judgements ***************************************)
Inductive TYPE : tyenv -> exp -> ty -> Prop :=
 | TYVar 
   :   forall tenv i T
   ,   get tenv i = Some T
   ->  TYPE tenv (XVar i) T  
       (* we want to know length of tenv i < length tenv
          makes it locally closed *)

 | TYLam
   :  forall tenv t T1 T2
   ,  TYPE (tenv :> T1) t T2
   -> TYPE tenv (XLam T1 t) (TFun T1 T2)

 | TYApp
   :  forall tenv t1 t2 T1 T2
   ,  TYPE tenv t1 (TFun T1 T2)
   -> TYPE tenv t2 T1
   -> TYPE tenv (XApp t1 t2) T2.

Hint Constructors TYPE.


(* Strengthening type environments. *************)
Theorem type_tyenv_weaken1
 :  forall tenv t T1 T2
 ,  TYPE tenv          t T1
 -> TYPE (T2 <: tenv)  t T1.
Proof.
 intros. gen tenv T1.
 induction t; intros; inversions H.

 Case "XVar".
  eapply TYVar. apply get_cons_some. auto.

 Case "XLam".
  eapply TYLam. rewrite snoc_cons.
  apply IHt. auto.

 Case "XApp".
  eapply TYApp; eauto.
Qed.


Theorem type_tyenv_weaken
 :  forall tenv1 tenv2 t1 T1
 ,  TYPE tenv1            t1 T1
 -> TYPE (tenv2 ++ tenv1) t1 T1.
Proof.
 intros. gen tenv1.
 induction tenv2; intros.
  rewrite append_empty_left. auto.
  rewrite append_snoc.  apply IHtenv2.
   apply type_tyenv_weaken1. auto.
Qed.


(* Strengthen type environments *****************)
Theorem type_tyenv_strengthen
 :  forall tenv tenv' n t T
 ,   coversX n t
 ->  tenv' = take n tenv
 ->  TYPE tenv  t T
 ->  TYPE tenv' t T.
Proof.
 intros. gen tenv tenv' n T.
 induction t; intros; inversions H1.
 
 Case "XVar".
  apply TYVar. inversions H.
  apply get_take. auto. auto.

 Case "XLam".
  eapply TYLam. inversions H.
  eapply IHt.  apply H2.
  assert (take n tenv :> t = take (S n) (tenv :> t)). simpl. auto.
  eauto. auto.

 Case "XApp".
  inversions H.
  eapply TYApp.
  eapply IHt1; eauto.
  eapply IHt2; eauto.
Qed.


(* Checking closed expressions ******************)
Theorem type_check_closed_in_empty
 :  forall tenv t T
 ,  closedX t
 -> TYPE tenv  t T
 -> TYPE Empty t T.
Proof.
 intros. inversions H.
 eapply type_tyenv_strengthen; eauto.
Qed.


Theorem type_check_closed_in_any
 :  forall tenv tenv' t1 T1
 ,  closedX t1
 -> TYPE tenv  t1 T1
 -> TYPE tenv' t1 T1.
Proof.
 intros.
 lets D: type_check_closed_in_empty H H0. clear H0.
 assert (TYPE (tenv' ++ Empty) t1 T1).
  apply type_tyenv_weaken. auto.
  simpl in H0. auto.
Qed.


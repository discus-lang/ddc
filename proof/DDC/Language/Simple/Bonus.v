(* Bonus lemmas that aren't used by the main proofs *)

Require Import DDC.Language.Simple.Exp.
Require Import DDC.Language.Simple.Ty.
Require Import DDC.Language.Simple.SubstExpExp.


(********************************************************************)
(* Weakening type environments *)
Lemma type_tyenv_weaken1
 :  forall te x t1 t2
 ,  TYPE te          x t1
 -> TYPE (t2 <: te)  x t1.
Proof.
 intros. gen te t1.
 induction_type x.

 Case "XLam".
  eapply TYLam. rewrite snoc_cons. auto.
Qed.


Lemma type_tyenv_weaken
 :  forall tenv1 tenv2 x1 t1
 ,  TYPE tenv1            x1 t1
 -> TYPE (tenv2 >< tenv1) x1 t1.
Proof.
 intros. gen tenv1.
 induction tenv2; intros.
  rewrite app_nil_right. auto.
  rewrite app_snoc.  apply IHtenv2.
   apply type_tyenv_weaken1. auto.
Qed.


(********************************************************************)
(* Strengthen type environments *)
Lemma type_tyenv_strengthen
 :  forall te te' n x t
 ,   wfX te' x
 ->  te' = firstn n te
 ->  TYPE te  x t
 ->  TYPE te' x t.
Proof.
 intros. gen te te' n t.
 induction_type x; subst. 

 Case "XVar".
  destruct H. eauto.

 Case "XLam".
  simpl in H.
  eapply TYLam.
   eapply IHx with (n := S n) (te := te :> t); auto.

 Case "XApp".
  simpl in H. burn.
Qed.


Lemma type_check_closed_in_empty_tyenv
 :  forall tenv x t
 ,  closedX x
 -> TYPE tenv x t
 -> TYPE nil  x t.
Proof.
 intros.
 eapply type_tyenv_strengthen; eauto.
  symmetry. eapply firstn_zero.
Qed.


Theorem type_check_closed_in_any_tyenv
 :  forall tenv tenv' x1 t1
 ,  closedX x1
 -> TYPE tenv  x1 t1
 -> TYPE tenv' x1 t1.
Proof.
 intros.
 assert (TYPE nil x1 t1).
  eapply type_check_closed_in_empty_tyenv; eauto.
 assert (TYPE (tenv' >< nil) x1 t1); eauto.
  apply type_tyenv_weaken. auto.
Qed.


(* If an expression is well formed under a given environment, 
   then all its indices are less than the length of this environment. 
   Lifting indices more than this length doesn't do anything *)
Theorem liftX_wfX
 :  forall d x e
 ,  d = length e
 -> wfX e x
 -> liftX d x = x.
Proof.
 intros. gen e d.
 induction x; intros; simpl; simpl in H0.
 
 Case "XVar".
  lift_cases; intros; eauto.
  false. subst. destruct H0.
   eapply get_above_false in H; auto. 

 Case "XLam".
  eapply IHx in H0; eauto.
  simpl in H0. symmetry. rewrite H. rewrite H0. auto.

 Case "XApp".
  destruct H0.
  spec IHx1 H0 H. rewrite IHx1.
  spec IHx2 H1 H. rewrite IHx2.
  auto.
Qed.


(* If an expression is closed then it has no free indices. 
   Lifting it doesn't do anything. *)
Lemma liftX_closed
 :  forall x
 ,  closedX x
 -> liftX 0 x = x.
Proof.
 intros.
  eapply liftX_wfX; eauto. auto.
Qed.



Require Import DDC.Language.SimpleRef.Step.
Require Import DDC.Language.SimpleRef.SubstExpExp.
Require Import DDC.Language.SimpleRef.Ty.

Definition extends (ys: stenv) (xs: stenv)
 := exists zs, ys = zs >< xs.

Lemma type_stenv_extends
 : forall te se1 se2 x t1
 ,  extends se2 se1
 -> TYPE te se1 x t1
 -> TYPE te se2 x t1.
Proof.
 intros.
 simpl in H.
 destruct H as [se3]. subst.
 auto.
Qed.
Hint Resolve type_stenv_extends.


(* If a closed, well typed expression takes an evaluation step 
   then the result has the same type as before. *)
Theorem preservation
 :  forall se h x h' x' t
 ,  TYPEH se h
 -> TYPE  nil se x  t
 -> STEP  h x h' x'
 -> (exists se', extends se' se /\ TYPE  nil se' x' t /\ TYPEH se' h').
Proof.
 intros se h x h' x' t HTH HT HS. gen t.
 induction HS; intros.

 Case "EsContext".
  specializes IHHS HTH.
  destruct H; try
   (inverts HT;
    edestruct IHHS as [se2]; eauto;
    exists se2;
    splits; try tauto;
    repeat (match goal with 
     | [ H : _ /\ _ |- _ ] => inverts H
    end); eauto).

 Case "EsLamApp".
  inverts HT.
  inverts H4.
  exists se. splits; auto.
   admit. (* extends is wrong *)
  eapply subst_exp_exp; eauto.

 Case "EsLamNewRef".
  inverts HT.
  exists (tData <: se). splits.
   admit. (* extends is wrong *)
   eapply TyLoc.
    admit. (* ok, list lemma *)
    admit. (* ok *)   

 Case "EsReadRef".
  inverts HT.
  inverts H3.
  exists se. splits.
  admit. (* extends *)
  admit. (* ok *)
  auto.
  admit.
Qed.


(* If a closed, well typed expression takes several evaluation steps
   then the result has the same type as before. *)
(*
Lemma preservation_steps
 :  forall x1 t1 x2
 ,  TYPE nil x1 t1
 -> STEPS    x1 x2
 -> TYPE nil x2 t1.
Proof.
 intros. 
 induction H0; eauto.
  eapply preservation; eauto.
Qed.
*)

(* If a closed, well typed expression takes several evaluation steps
   then the result has the same type as before. 
   Usses the left linearised version of steps judement. *)
(*
Lemma preservation_stepsl
 :  forall x1 t1 x2
 ,  TYPE nil x1 t1
 -> STEPSL   x1 x2
 -> TYPE nil x2 t1.
Proof.
 intros. 
 induction H0.
  auto.
  apply IHSTEPSL.
  eapply preservation. 
   eauto. auto.
Qed.
*)



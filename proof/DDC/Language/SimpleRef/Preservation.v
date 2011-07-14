
Require Import DDC.Language.SimpleRef.Step.
Require Import DDC.Language.SimpleRef.SubstExpExp.
Require Import DDC.Language.SimpleRef.Ty.

Definition extends {A: Type} (xs: list A) (ys: list A)
 := exists zs, xs = zs >< ys.


Lemma extends_refl
 : forall {A: Type} (se: list A)
 , extends se se.
Proof.
 induction se; intros.
  unfold extends. exists (@nil A). eauto.
  unfold extends. exists (@nil A). eauto.
Qed.
Hint Resolve extends_refl.


Lemma extends_snoc 
 : forall {A: Type} x (xx: list A)
 , extends (x <: xx) xx.
Proof.
 intros. unfold extends.
 exists (x :: nil).
 induction xx.
  simpl. auto.
  simpl. rewrite IHxx. auto.
Qed.
Hint Resolve extends_snoc. 


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
 -> (exists se', extends se' se 
              /\ TYPEH se' h'
              /\ TYPE  nil se' x' t).
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
  inverts_type.
  exists se. splits; auto.
  eapply subst_exp_exp; eauto.

 Case "EsLamNewRef".
  inverts_type.
  exists (tData <: se). splits.
   eauto.
   eapply Forall2_snoc.
    eapply type_stenv_push. auto.
    eapply Forall2_impl. 
    eapply type_stenv_push. auto. 
   eapply TyLoc.
    admit. (* ok, list lemma *)

 Case "EsReadRef".
  inverts_type.
  exists se. splits; auto.
  eapply Forall2_get_get_same; eauto.

 Case "EsWriteRef".
  inverts_type.
  exists se. splits; eauto.
  eapply Forall2_update_right; eauto.
  admit. (* ok, need type rule for unit *)
Qed.


(* If a closed, well typed expression takes several evaluation steps
   then the result has the same type as before. 
   Usses the left linearised version of steps judement. *)
Lemma preservation_stepsl
 :  forall se h x t h' x'
 ,  TYPEH se h
 -> TYPE  nil se x t
 -> STEPSL h x h' x'
 -> (exists se', extends se' se
              /\ TYPEH se' h'
              /\ TYPE  nil se' x' t).
Proof.
 intros se h x t h' x' HTH HT HS. gen se.
 induction HS; intros.
  Case "EslNone".
   eauto.
  Case "EslCons".
   lets D: preservation HTH HT H.
    destruct D as [se2].
    inverts H0. inverts H2.
   spec IHHS H0 H3.
    destruct IHHS as [se3].
    inverts H2. inverts H5.
    exists se3. splits; auto.
    admit. (* ok, need trans of extends *)
Qed.


(* If a closed, well typed expression takes several evaluation steps
   then the result has the same type as before. *)
Lemma preservation_steps
 :  forall se h x t h' x'
 ,  TYPEH se h
 -> TYPE  nil se x t
 -> STEPS h x h' x'
 -> (exists se', extends se' se
              /\ TYPEH se' h'
              /\ TYPE  nil se' x' t).
Proof.
 intros se h x t h' x' HTH HT HS.
 eapply stepsl_of_steps in HS.
 eapply preservation_stepsl; eauto.
Qed.


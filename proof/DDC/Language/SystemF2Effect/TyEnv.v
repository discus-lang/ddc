
Require Import DDC.Language.SystemF2Effect.TySubst.
Require Import DDC.Language.SystemF2Effect.TyLift.
Require Import DDC.Language.SystemF2Effect.TyWfT.
Require Import DDC.Language.SystemF2Effect.TyExp.


(* Type Enviroments *)
Definition tyenv := list ty.
Hint Unfold tyenv.


(* Lift type indices in type environments. *)
Definition liftTE d te    := map (liftTT 1 d) te.
Hint Unfold liftTE.

(* Substitution of types in type environments. *)
Definition substTE d t te := map (substTT d t) te.
Hint Unfold substTE.


(********************************************************************)
Lemma length_liftTE
 :  forall d te
 ,  length te = length (liftTE d te).
Proof.
 induction te; burn.
Qed.


(********************************************************************)
Lemma liftTE_liftTE
 :  forall n n' te
 ,  liftTE n              (liftTE (n + n') te) 
 =  liftTE (1 + (n + n')) (liftTE n te).
Proof. 
 intros. induction te; rip.
  unfold liftTE.
  simpl. rewrite liftTT_liftTT_11. burn.
Qed.


Lemma substTE_liftTE
 :  forall d te t2
 ,  substTE d t2 (liftTE d te) = te.
Proof.
 intros.
 unfold substTE. unfold liftTE.
 lists.
 induction te; simpl; burn.
Qed.


Lemma liftTE_substTE
 :  forall n n' t2 te
 ,  liftTE n (substTE (n + n') t2 te)
 =  substTE (1 + n + n') (liftTT 1 n t2) (liftTE n te).
Proof.
 intros. induction te; rip.
  unfold substTE. unfold liftTE.
   simpl. rewrite liftTT_substTT.
   unfold liftTE  in IHte.
   unfold substTE in IHte. rewrite IHte. auto.
Qed.


Lemma liftTE_closedT_id
 :  forall n se
 ,  Forall closedT se
 -> liftTE n se = se.
Proof.
 intros.
 unfold liftTE.
 induction se; rip.
  inverts H. rip.
  rs. rw (liftTT 1 n a = a).
  auto.
Qed.
Hint Resolve liftTE_closedT_id.


Lemma substTE_closedT_id
 :  forall n t2 se
 ,  Forall closedT se
 -> substTE n t2 se = se.
Proof.
 intros.
 unfold substTE.
 induction se; rip.
  inverts H. rip.
  rs. rw (substTT n t2 a = a).
  auto.
Qed.
Hint Resolve substTE_closedT_id.


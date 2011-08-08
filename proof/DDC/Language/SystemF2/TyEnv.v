
Require Export DDC.Language.SystemF2.Ty.


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
 intros. 
 induction te.
  eauto.
  simpl. burn.
Qed.


(********************************************************************)
Lemma liftTE_liftTE
 :  forall n n' te
 ,  liftTE n              (liftTE (n + n') te) 
 =  liftTE (1 + (n + n')) (liftTE n te).
Proof. 
 intros. induction te.
  auto.
  unfold liftTE.
   simpl. rewrite liftTT_liftTT_11.
   unfold liftTE in IHte. rewrite IHte. auto.
Qed.


Lemma substTE_liftTE
 :  forall d te t2
 ,  substTE d t2 (liftTE d te) = te.
Proof.
 intros.
 unfold substTE. unfold liftTE.
 rewrite map_map.
 unfold Basics.compose.
 induction te. 
  auto.
  simpl. rewrite substTT_liftTT. rewrite IHte. auto.
Qed.


Lemma liftTE_substTE
 :  forall n n' t2 te
 ,  liftTE n (substTE (n + n') t2 te)
 =  substTE (1 + n + n') (liftTT 1 n t2) (liftTE n te).
Proof.
 intros. induction te.
  auto.
  unfold substTE. unfold liftTE.
   simpl. rewrite liftTT_substTT.
   unfold liftTE in IHte.
   unfold substTE in IHte. rewrite IHte. auto.
Qed.


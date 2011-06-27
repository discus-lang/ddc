
Require Export DDC.Language.SystemF.Ty.


(* Type enviroments *)
Definition tyenv := list ty.
Hint Unfold tyenv.


(* Lift type indices in type environments. *)
Definition liftTE d te    := map (liftTT d) te.
Hint Unfold liftTE.


(* Substitution of Types in Type Environments. *)
Definition substTE d t te := map (substTT d t) te.
Hint Unfold substTE.


(********************************************************************)
(* The following lemmas as similar to the ones in Ty.v, 
   except that we've applied them to entire type environments. 
 *)

Lemma liftTE_liftTE
 :  forall n n' te
 ,  liftTE n              (liftTE (n + n') te) 
 =  liftTE (1 + (n + n')) (liftTE n te).
Proof. 
 intros. induction te.
  auto.
  unfold liftTE.
   simpl. rewrite liftTT_liftTT.
   unfold liftTE in IHte. rewrite IHte. auto.
Qed.


Lemma substTE_liftTE
 :  forall d te t2
 ,  substTE d t2 (liftTE d te) = te.
Proof.
 intros.
 unfold substTE.
 unfold liftTE.
 rewrite map_map.
 induction te. 
  auto.
  simpl. 
   unfold compose. rewrite substTT_liftTT.
   unfold compose in IHte. rewrite IHte. auto.
Qed.


Lemma liftTE_substTE
 :  forall n n' t2 te
 ,  liftTE n (substTE (n + n') t2 te)
 =  substTE (1 + n + n') (liftTT n t2) (liftTE n te).
Proof.
 intros. induction te.
  auto.
  unfold substTE. unfold liftTE.
   simpl. rewrite liftTT_substTT.
   unfold liftTE in IHte.
   unfold substTE in IHte. rewrite IHte. auto.
Qed.


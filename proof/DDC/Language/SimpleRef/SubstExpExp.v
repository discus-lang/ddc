
Require Import DDC.Language.SimpleRef.Ty.
Require Export DDC.Language.SimpleRef.Exp.

(* Substitution of expressions in expressions preserves typing.

   Inductively, we must reason about performing substitutions at any
   depth, hence we must prove a property about (subst' d x2 x1) instead
   of the weaker (subst x2 x1) which assumes the substitution is taking
   place at top level. *)
Lemma subst_exp_exp_ix
 :  forall ix te se x1 x2 t1 t2
 ,  get  ix te = Some t2
 -> TYPE te se             x1 t1
 -> TYPE (delete ix te) se x2 t2
 -> TYPE (delete ix te) se (substX ix x2 x1) t1.
Proof.
 intros. gen ix te se x2 t1.
 induction x1; intros; simpl; inverts H0; eauto.

 Case "XVar".
  fbreak_nat_compare.
  SCase "i = ix".
   rewrite H in H5. inverts H5. auto.

  SCase "n < ix".
   apply TyVar.
   rewrite <- H5.
    apply get_delete_above. auto.

  SCase "n > ix".
   apply TyVar.
   destruct n.
    false. omega.
    simpl. nnat. rewrite <- H5.
     apply get_delete_below. omega.

 Case "XLam".
  apply TyLam.
  rewrite delete_rewind.
  apply IHx1; auto.
   simpl. apply type_tyenv_weaken. auto.
Qed.


Theorem subst_exp_exp
 :  forall te se x1 x2 t1 t2
 ,  TYPE (te :> t2) se x1 t1
 -> TYPE te se         x2 t2 
 -> TYPE te se (substX 0 x2 x1) t1.
Proof. 
 intros te se x1 x2 t1 t2 Ht1 Ht2.
 lets H: subst_exp_exp_ix 0 (te :> t2).
  simpl in H. eauto.
Qed.


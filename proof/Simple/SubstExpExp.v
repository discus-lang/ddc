
Require Import TyJudge.
Require Export Exp.

(* Substitution of values in values. 
   Inductively, we must reason about performing substitutions at any
   depth, hence we must prove a property about (subst' d x2 x1) instead
   of the weaker (subst x2 x1) which assumes the substitution is taking
   place at top level.
 *)
Theorem subst_value_value_ix
 :  forall ix e x1 x2 t1 t2
 ,  get  e ix = Some t2
 -> TYPE e           x1 t1
 -> TYPE (drop ix e) x2 t2
 -> TYPE (drop ix e) (substX ix x2 x1) t1.
Proof.
 intros. gen ix e x2 t1.
 induction x1; intros; simpl; inverts H0; eauto.

 Case "XVar".
  fbreak_nat_compare.
  SCase "i = ix".
   rewrite H in H4. inverts H4. auto.

  SCase "n < ix".
   apply TYVar.
   rewrite <- H4.
    apply get_drop_above. auto.

  SCase "n > ix".
   apply TYVar.
   destruct n.
    false. omega.
    simpl. nnat. rewrite <- H4.
     apply get_drop_below. omega.

 Case "XLam".
  apply TYLam.
  rewrite drop_rewind.
  apply IHx1; auto.
   simpl. apply type_tyenv_weaken. auto.
Qed.


Theorem subst_value_value
 :  forall tenv x1 x2 t1 t2
 ,  TYPE (tenv :> t2) x1 t1
 -> TYPE tenv         x2 t2 
 -> TYPE tenv (substX 0 x2 x1) t1.
Proof. 
 intros tenv x1 x2 t1 t2 Ht1 Ht2.
 lets H: subst_value_value_ix 0 (tenv :> t2).
  simpl in H. eauto.
Qed.


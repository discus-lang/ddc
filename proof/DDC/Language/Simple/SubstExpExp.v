
Require Import DDC.Language.Simple.Ty.
Require Export DDC.Language.Simple.Exp.
Require Import Coq.Logic.FunctionalExtensionality.


(* Substitution of expressions in expressions preserves typing.

   Inductively, we must reason about performing substitutions at any
   depth, hence we must prove a property about (subst' d x2 x1) instead
   of the weaker (subst x2 x1) which assumes the substitution is taking
   place at top level. *)
Lemma subst_exp_exp_ix
 :  forall ix te x1 x2 t1 t2
 ,  get  ix te = Some t2
 -> TYPE te             x1 t1
 -> TYPE (delete ix te) x2 t2
 -> TYPE (delete ix te) (substX ix x2 x1) t1.
Proof.
 rip. gen ix te x2 t1.
 induction_type x1.

 Case "XVar".
  fbreak_nat_compare; try burn.
  SCase "n > ix".
   eapply TYVar.
   destruct n.
    burn.
    simpl. nnat. rewrite <- H3.
    apply get_delete_below. burn.

 Case "XLam". 
  apply TYLam.
  rewrite delete_rewind.
  apply IHx1; burn.
Qed.


Theorem subst_exp_exp
 :  forall te x1 x2 t1 t2
 ,  TYPE (te :> t2) x1 t1
 -> TYPE te         x2 t2 
 -> TYPE te (substX 0 x2 x1) t1.
Proof.
 rip. lets D: subst_exp_exp_ix 0 (te :> t2).
 simpl in D. eauto.
Qed.


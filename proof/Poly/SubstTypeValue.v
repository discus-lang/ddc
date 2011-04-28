
Require Import SubstTypeType.
Require Import TyJudge.
Require Import KiJudge.
Require Import Exp.
Require Import Env.
Require Import Base.


(* Lift type indices that are at least a certain depth. *)
Fixpoint liftTX' (d: nat) (xx: exp) : exp :=
  match xx with
  |  XVar _     => xx

  |  XLAM x     
  => XLAM (liftTX' (S d) x)

  |  XAPP x t 
  => XAPP (liftTX' d x)  (liftTT' d t)
 
  |  XLam t x   
  => XLam (liftTT' d t)  (liftTX' d x)

  |  XApp x1 x2
  => XApp (liftTX' d x1) (liftTX' d x2)
 end.


(* Substitution of Types in Exps *)
Fixpoint substTX' (d: nat) (u: ty) (xx: exp) : exp :=
  match xx with
  | XVar _     => xx

  |  XLAM x     
  => XLAM (substTX' (S d) (liftTT' 0 u) x)

  |  XAPP x t
  => XAPP (substTX' d u x)  (substTT' d u t)

  |  XLam t x
  => XLam (substTT' d u t)  (substTX' d u x)

  |  XApp x1 x2
  => XApp (substTX' d u x1) (substTX' d u x2)
 end.


Definition  substTX := substTX' 0.
Hint Unfold substTX.


Theorem subst_type_value_drop
 :  forall ix ke te x1 t1 t2 k2
 ,  get ke ix = Some k2
 -> TYPE ke  te x1 t1
 -> KIND (drop ix ke)  t2 k2
 -> TYPE (drop ix ke) 
         (substTE' ix t2 te)
         (substTX' ix t2 x1)
         (substTT' ix t2 t1).
Proof.
 intros. gen ix ke te t1 t2 k2.
 induction x1; intros; simpl; inverts H0; eauto.

 Case "XVar".
  apply TYVar. admit. (* ok, lemma about map *)

 Case "XLAM".
  simpl. apply TYLAM.
  rewrite drop_rewind.
  
  assert (liftTE (substTE' ix t2 te) = substTE' (S ix) (liftTT' 0 t2) (liftTE' 0 te)).
    admit. rewrite H0. (* ok, instance of liftTT_substTT *)

  eapply IHx1; eauto.
   apply liftTT_push. auto.

 Case "XAPP".
  rewrite (substTT_substTT 0 ix).
  apply TYAPP.
   simpl. eapply (IHx1 ix) in H6; eauto.
   simpl. eapply subst_type_type_drop; eauto.
  
 Case "XLam".
  simpl. apply TYLam.
  assert ( substTE' ix t2 te :> substTT' ix t2 t
         = substTE' ix t2 (te :> t)). admit. (* ok, prop of substTE' *)
         rewrite H0. clear H0.
  eapply IHx1; eauto.

 Case "XApp".
  eapply TYApp.
   eapply IHx1_1 in H6; eauto.
   eapply IHx1_2 in H8; eauto.
Qed.



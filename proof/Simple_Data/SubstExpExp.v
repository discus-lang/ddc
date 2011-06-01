
Require Import TyJudge.
Require Export Exp.

(* Substitution of values in values. 
   Inductively, we must reason about performing substitutions at any
   depth, hence we must prove a property about (subst' d x2 x1) instead
   of the weaker (subst x2 x1) which assumes the substitution is taking
   place at top level.
 *)
Theorem subst_value_value_ix
 :  forall ix ds te x1 x2 t1 t2
 ,  get  te ix = Some t2
 -> TYPE ds te           x1 t1
 -> TYPE ds (drop ix te) x2 t2
 -> TYPE ds (drop ix te) (substX ix x2 x1) t1.
Proof.
 intros. gen ix ds te x2 t1 t2.

 eapply 
  (exp_mutind 
    (fun x1 => forall ix ds te x2 t1
            ,  TYPE ds te           x1 t1
            -> forall t2
            ,  get te ix = Some t2
            -> TYPE ds (drop ix te) x2 t2
            -> TYPE ds (drop ix te) (substX ix x2 x1) t1)

    (fun a1 => forall ix ds te x2 t11 t12 t2
            ,  get te ix = Some t2
            -> TYPEA ds te           a1 t11 t12
            -> TYPE  ds (drop ix te) x2 t2
            -> TYPEA ds (drop ix te) (substA ix x2 a1) t11 t12))
  ; intros; simpl.

 Case "XVar".
  inverts H.
  fbreak_nat_compare.
  SCase "i = ix".
   rewrite H0 in H5. inverts H5. auto.

  SCase "n < ix".
   apply TYVar.
   rewrite <- H5.
    apply get_drop_above. auto.

  SCase "n > ix".
   apply TYVar.
   destruct n.
    false. omega.
    simpl. nnat. rewrite <- H5.
     apply get_drop_below. omega.

 Case "XLam".
  inverts H0.
  apply TYLam.
  rewrite drop_rewind.
  eapply H; eauto.
   simpl. apply type_tyenv_weaken. auto.

 Case "XApp". 
  inverts H1. eauto.

 Case "XCon".
  inverts H0.
  eapply TYCon.
   eauto.
   rewrite Forall_forall in H.
   apply (Forall2_map_left (TYPE ds (drop ix te))).
   apply (Forall2_impl_In  (TYPE ds te)); eauto.

 Case "XCase".
  inverts H1.
  eapply TYCase.
   eauto.
   rewrite Forall_forall in H0.
   eapply Forall_map.
   eapply (Forall_impl_In (fun a => TYPEA ds te a tPat t1)); eauto.

 Case "AAlt".
  inverts H1.
  eapply TYAlt. 
   eauto.
   assert  ( drop ix  te ++ envOfList ts
           = drop (ix + length (envOfList ts)) (te ++ envOfList ts)).
    admit. rewrite H1. clear H1.
   simpl.
   (* TODO: Definition of substA is broken.
            Need to handle all of the ctor args being pushed
            onto the environment at onceee.
            Generalise liftX to lift across several new types at once *)
   eapply H.
    eauto.
    eapply get_append_some. eauto.
    

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


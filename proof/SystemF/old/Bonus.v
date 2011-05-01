
(* Substitution of Types in Environments.
   The new type corresponds to one of the kinds in the environment.
   We drop out that kind, and substitute the new type into all
   the indices that were pointing to the kind.
   The susbstitution process also adjusts the indices that were
   pointing to kinds that appear in the environment after the one
   that was dropped.

   Example:
       k3, k2, (0 -> 1), k1, k0, (1 -> 2)    [C/2]
   =>  k3, (C -> 0), k1, k0, (1 -> C)        [D/1]
   =>  k3, (C -> 0), k0, (D -> C) 
 *)
Fixpoint substDropTE (n: nat) (u: ty) (e: tyenv) : tyenv :=
  match n, e with 
  |    _, Empty             => Empty

  (* Found the kind to drop, we're done. *)
  |    0, Snoc e' (EKind k) => e'

  (* Still looking for the kind to drop out. *)
  | S n', Snoc e'                    (EKind k) 
       => Snoc (substDropTE n' u e') (EKind k)

  (* As we pass over types in the environment,
     substitute in the new one *)
  |    O, Snoc e'                    (EType t) 
       => Snoc (substDropTE n  u e') (EType (substTT' n u t))

  |    _, Snoc e'                    (EType t) 
       => Snoc (substDropTE n  u e') (EType (substTT' n u t))
  end.


(* Weakening kind environment ***************************************)

(* If we add a new kind to the environment of a well kinded type, 
   then that type is still well kinded *)
Lemma kind_weaken1
 :  forall e t1 k1 l
 ,  KIND e        t1 k1
 -> KIND (l <: e) t1 k1.
Proof.
 intros. gen e k1.
 induction t1; intros; inverts H; eauto.

 Case "TVar".
  apply KIVar. 
  apply getMatch_cons_some. auto.

 Case "TForall".
  apply KIForall.
  rewrite snoc_cons. apply IHt1. auto.
Qed.


Lemma kind_weaken
 :  forall e1 e2 t1 k1
 ,  KIND e1         t1 k1
 -> KIND (e2 ++ e1) t1 k1.
Proof.
 intros. gen e1 k1.
 induction e2; intros.
  rewrite append_empty_left. auto.
  rewrite append_snoc. apply IHe2.
   apply kind_weaken1. auto.
Qed.


(* Strenghten kind environment **************************************)

(* If a well kinded type doesn't use elements on the end of its
   environment, then it is still well kinded if we cut them off *)
Lemma kind_strengthen
 :  forall e e' n t k
 ,   wfT e' t
 ->  e' = take n e
 ->  KIND e  t k
 ->  KIND e' t k.
Proof.
 intros. gen e e' n k.
 induction t; intros; inverts H1; eauto.
 Case "TVar".
  apply KIVar. subst.
  destruct H. admit. (* ok, list lemma *)

 Case "TForall".
  apply KIForall. subst.
  eapply IHt with (n := S n) (e := e :> EKind KStar); auto.

 Case "TFun".
  simpl in H. destruct H. eauto. 
Qed.




(* If a closed type is well kinded,
   then it is well kinded in an empty environment. *)
Lemma kind_closed_in_empty
 :  forall e t k
 ,  closedT t
 -> KIND e     t k
 -> KIND Empty t k.
Proof.
 intros. unfold closedT in H.
 eapply kind_strengthen; eauto.
 eapply take_zero.
Qed.


(* If a closed type is well kinded,
   then it is well kinded in an any environment. *)
Theorem kind_closed_in_any
 :  forall e e' t k
 ,  closedT t
 -> KIND e  t k
 -> KIND e' t k.
Proof.
 intros.
 lets D: kind_closed_in_empty H H0.
 assert (KIND (e' ++ Empty) t k).
  apply kind_weaken. auto.
  auto.
Qed.


(* The environment used to type an expression is well formed *)
Theorem type_wfEnv
 :  forall kenv tenv x t
 ,  TYPE kenv tenv x t
 -> wfEnv kenv tenv.
Proof.
 intros. gen kenv tenv t.
 induction x; intros.

 Case "XVar".
  inverts H. auto.

 Case "XLAM".
  inverts H. apply IHx in H3. admit.

 Case "XAPP".
  inverts H. eauto.

 Case "XLam".
  inverts H. apply IHx in H5. simpl in H5. tauto.

 Case "XApp".
  inverts H. eauto. 
Qed.


(* A well typed expression is well formed *)
Theorem type_wfX
 :  forall kenv tenv x t
 ,  TYPE kenv tenv x t
 -> wfX  kenv tenv x /\ wfT kenv t.
Proof.
 intros. gen kenv tenv t.
 induction x; intros; simpl; eauto.

 Case "XVar".
  inverts H. split. eauto.
  eapply wfT_from_wfEnv; eauto.

 Case "XLAM".
  inverts H. apply IHx in H3. eauto.

 Case "XAPP".
  inverts H. apply IHx in H4. destruct H4.
   split. split. auto.
   eapply kind_wfT. eauto.
   simpl in H0.
   admit. (* subst types still well formed *)

 Case "XLam".
  inverts H. 
  lets D: IHx H5. destruct D.
  split. split.
   admit. 
   auto.
   simpl. split.
    admit.
    auto.

 Case "XApp".
  inverts H.
  apply IHx1 in H4. destruct H4.
  apply IHx2 in H6. destruct H6.
  simpl in H0. tauto.
Qed. 


(* Weakening type environment ***************************************)
Lemma type_tyenv_weaken1
 :  forall kenv tenv x1 t1 t2
 ,  TYPE kenv tenv         x1 t1
 -> TYPE kenv (t2 <: tenv) x1 t1.
Proof.
 intros. gen kenv tenv t1.
 induction x1; intros; inverts H; eauto.

 Case "XLam".
  apply TYLam. rewrite snoc_cons. auto.
Qed.


Lemma type_tyenv_weaken
 :  forall kenv tenv1 tenv2 x1 t1
 ,  TYPE kenv tenv1            x1 t1
 -> TYPE kenv (tenv2 ++ tenv1) x1 t1.
Proof.
 intros. gen kenv tenv1 t1.
 induction tenv2; intros.
  rewrite append_empty_left. auto.
  rewrite append_snoc. apply IHtenv2.
   apply type_tyenv_weaken1. auto.
Qed.


(* Weakening kind environment ***************************************)
Lemma type_kienv_weaken1
 :  forall kenv tenv x1 t1 k1
 ,  TYPE kenv tenv         x1 t1
 -> TYPE (k1 <: kenv) tenv x1 t1.
Proof.
 intros. gen kenv tenv t1.
 induction x1; intros; inverts H; eauto.

 Case "XLAM".
  apply TYLAM. rewrite snoc_cons. auto.

 Case "XAPP".
  apply TYAPP; auto.
  apply kind_kienv_weaken1. auto.
Qed.


Lemma type_kienv_weaken
 :  forall kenv1 kenv2 tenv x t
 ,  TYPE kenv1            tenv x t
 -> TYPE (kenv2 ++ kenv1) tenv x t.
Proof.
 intros. gen kenv1 tenv t.
 induction kenv2; intros.
  rewrite append_empty_left. auto.
  rewrite append_snoc. apply IHkenv2.
   apply type_kienv_weaken1. auto.
Qed.


(* Strenghten type environment **************************************)
Lemma type_tyenv_strengthen
 :  forall kenv tenv tenv' n x t
 ,   coversXX n x
 ->  tenv' = take n tenv
 ->  TYPE kenv tenv  x t
 ->  TYPE kenv tenv' x t.
Proof.
 intros. gen kenv tenv tenv' n t.
 induction x; intros; inverts H1; inverts H; eauto.

 Case "XVar".
  apply TYVar. subst.
  apply get_take; auto.

 Case "XLam".
  apply TYLam. subst.
  eapply IHx with (n := S n) (tenv := tenv :> t); auto.
Qed.


(* Strenghten kind environment **************************************)
Lemma type_kienv_strengthen
 :  forall kenv kenv' tenv n x t
 ,   coversTX n x
 ->  kenv' = take n kenv
 ->  TYPE kenv  tenv x t
 ->  TYPE kenv' tenv x t.
Proof.
 intros. gen kenv kenv' tenv n t.
 induction x; intros; inverts H1; inverts H; eauto.

 Case "XLAM".
  apply TYLAM. subst.
  apply IHx with (n := S n) (kenv := kenv :> KStar); auto.

 Case "XAPP".
  apply TYAPP. eauto. subst.
  eapply kind_kienv_strengthen; auto. auto.
Qed.


(* Checking closed expressions **************************************)

(* Closure under type environment ***************)
Lemma type_check_closedUnderXX
 :  forall kenv tenv x t
 ,  TYPE   kenv tenv x t
 -> closedUnderXX tenv x.
Proof.
 intros. eapply ClosedUnderXX. gen kenv tenv t.
 induction x; intros; inverts H; eauto.

 Case "XLam".
  apply IHx in H5. auto.
Qed.


Lemma type_check_empty_tyenv_is_tyclosed
 :  forall kenv     x t
 ,  TYPE kenv Empty x t 
 -> closedXX x.
Proof.
 intros. eapply type_check_closedUnderXX in H.
 inverts H. auto.
Qed.


Lemma type_check_tyclosed_in_empty_tyenv
 :  forall kenv tenv x t
 ,  closedXX x 
 -> TYPE kenv tenv  x t
 -> TYPE kenv Empty x t.
Proof.
 intros. inverts H.
 eapply type_tyenv_strengthen; eauto.
Qed.


Lemma type_check_tyclosed_in_any_tyenv
 :  forall kenv tenv tenv' x t
 ,  closedXX x
 -> TYPE kenv tenv  x t
 -> TYPE kenv tenv' x t.
Proof.
 intros.
 lets D: type_check_tyclosed_in_empty_tyenv H H0.
 assert (TYPE kenv (tenv' ++ Empty) x t).
  apply type_tyenv_weaken. auto.
  auto.
Qed.


(* Closure under kind environment ***************)
Lemma type_check_closedUnderTX
 :  forall kenv tenv x t
 ,  TYPE kenv tenv x t
 -> closedUnderTX kenv x.
Proof.
 intros. eapply ClosedUnderTX. gen kenv tenv t.
 induction x; intros; inverts H; eauto.

 Case "XLAM".
  apply IHx in H3. auto.

 Case "XAPP".
  apply CoversTX_APP. eapply IHx; eauto.
  apply kind_check_closedUnderTT in H6. inverts H6. auto.
Qed.


Lemma type_check_empty_kienv_is_kiclosed
 :  forall tenv x t
 ,  TYPE Empty tenv x t
 -> closedTX x.
Proof.
 intros. eapply type_check_closedUnderTX in H.
 inverts H. auto.
Qed.


Lemma type_check_kiclosed_in_empty_kienv
 :  forall kenv tenv x t
 ,  closedTX x
 -> TYPE kenv  tenv x t
 -> TYPE Empty tenv x t.
Proof.
 intros. inverts H.
 eapply type_kienv_strengthen; eauto.
Qed.


Lemma type_check_kiclosed_in_any_kienv
 :  forall kenv kenv' tenv x t
 ,  closedTX x
 -> TYPE kenv  tenv x t
 -> TYPE kenv' tenv x t.
Proof.
 intros.
 lets D: type_check_kiclosed_in_empty_kienv H H0.
 assert (TYPE (kenv' ++ Empty) tenv x t).
  apply type_kienv_weaken. auto.
  auto.
Qed.

(* Lifting a type index by zero steps doesn't do anything. *)
Theorem liftTX_zero
 :  forall x depth
 ,  liftTX 0 depth x = x.
Proof.
 induction x; intros; simpl;
  try (auto);
  try (rewrite IHx;  auto);
  try (rewrite IHx1; auto; rewrite IHx2; auto);
  try (rewrite liftTT_zero; auto).
Qed.


(* Lifting covered indices doesn't do anything. *)
Theorem liftTX_covers
 :  forall ix n t
 ,  coversTX n t
 -> liftTX ix n t = t.
Proof.
 intros ix n t.
 gen n.
 induction t; intros; inverts H; simpl;
  try auto;
  try (rewrite IHt; auto);
  try (rewrite IHt1; auto; rewrite IHt2; auto);
  try (rewrite liftTT_covers; auto).
Qed.


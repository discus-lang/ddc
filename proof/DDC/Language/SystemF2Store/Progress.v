
Require Import DDC.Language.SystemF2Store.Step.
Require Import DDC.Language.SystemF2Store.TyJudge.
Require Import DDC.Language.SystemF2Store.Exp.
Require Import DDC.Base.


(* If we have a well typed case match on a store location containing some 
   data object, then there is a case alternative corresponding to
   that object's data constructor. *)

(* TODO: 
   Well typed loc implies we have a heap binding
   Want TYPE (XLoc l) -> (exists dc svs, get l ss = Some dc svs) *)

Lemma getAlt_has
 :  forall ds se ss l dc svs alts t
 ,  WfS ds se ss
 -> get l ss = Some (SObj dc svs) (* TODO: drop this premise *)
 -> TYPE ds nil nil se (XCase (XLoc l) alts) t
 -> (exists x, getAlt dc alts = Some (AAlt dc x)).
Proof.
 intros.
 eapply getAlt_exists.
 inverts_type.
 nforall. 
 assert (In dc dcs).
  rewrite H6 in H3. inverts H3.
  have (getCtorOfType (TCon tc) = Some tc) as HC.
  inverts H. int.
  unfold STORET in H4.
   spec H4 H0.
   dest tcObj. dest tsParam. dest tsFields.
   int.
   rewrite H4 in H2. inverts H2.
   erewrite getCtorOfType_makeTApps with (tc := tcObj) in H6; eauto.
    inverts H6.
   eauto.
 eauto.
Qed.
Hint Resolve getAlt_has.



(* A well typed expression is either a well formed value, 
   or can transition to the next state. *)
Theorem progress
 :  forall ds se s x t
 ,  WfS ds se s
 -> TYPE ds nil nil se x t
 -> value x \/ (exists s' x', STEP s x s' x').
Proof.
 intros. gen t.
 induction x using exp_mutind with 
  (PA := fun a => a = a)
  ; intros.


 Case "XVar".
  nope.

 Case "XLoc".
  left. eauto 6.

 Case "XLAM".
  left. apply type_wfX in H0. eauto.


 Case "XAPP".
  inverts keep H0.
  edestruct IHx. eauto.
  SCase "x value".
   right. inverts H1. inverts H3.
    SSCase "x = XVar". nope.
    SSCase "x = XLoc". nope.
    SSCase "x = XLAM". exists s. exists (substTX 0 t2 x1). eapply EsLAMAPP.
    SSCase "x = XAPP". nope.
    SSCase "x = XApp". nope.
    SSCase "x = XCon".
     inverts_type.
     have (takeTCon (TCon tc0) = takeTCon (TForall t0)). nope.
    SSCase "x = XCase".
     nope.
  SCase "x steps".
   right.
    shift s'. dest x'.
    exists (XAPP x' t2).
    lets D: EsContext XcAPP H1.
    eauto.


 Case "XLam".
  left. eapply type_wfX in H0. eauto.


 Case "XApp".
  right.
  inverts_type.
  edestruct IHx1; eauto.
  SCase "value x1".
   edestruct IHx2; eauto.
    SSCase "value x2".
     have (exists t x, x1 = XLam t x) as HF.
     destruct HF as [t11].
     destruct H2 as [x12].
     subst.
     exists s.
     exists (substXX 0 x2 x12). 
     apply EsLamApp; eauto.
    SSCase "x2 steps".
     shift s'.
     destruct H1 as [x2'].
     exists (XApp x1 x2'). auto.
  SCase "x1 steps".
   shift s'.
   destruct H0  as [x1'].
   exists (XApp x1' x2).
   eapply (EsContext (fun xx => XApp xx x2)); auto.


 Case "XCon".
  inverts_type.
  (* All ctor args are either wnf or can step *)
  assert (Forall (fun x => wnfX x \/ (exists s' x', STEP s x s' x')) xs) as HWS.
   nforall. intros.
   have (exists t, TYPE ds nil nil se x t). dest t.
   have (value x \/ (exists s' x', STEP s x s' x')). int.     

  (* All ctor args are wnf, or there is a context where one can step *)
  lets D: (@exps_ctx_run exp exp) HWS.
  inverts D.
   (* All ctor args are wnf *)
   right.
   assert (Forall (wfT 0) ts).
    rrwrite (0 = length (@nil ki)).
    eapply kind_wfT_Forall2. eauto.

   assert (Forall (wfX 0 0 (length se)) xs).
    have    (0 = length (@nil ki)) as HKL. rewrite HKL at 1.
    rrwrite (0 = length (@nil ty)). eauto.

   assert (exists svs, Forall2 svalueOf xs svs).
    eapply (Forall2_exists_right_all exp svalue value). auto.
    assert (Forall closedX xs).
     nforall. eauto.
     nforall. eauto.
     dest svs.

   exists (snoc (SObj dc svs) s).
   exists (XLoc (length s)).
   eauto.

   (* There is a context where one ctor arg can step *)
   right.
    dest C. dest x'.
    int. subst.
    dest s'.
    lets D: step_context_XCon_exists H2 H1.
    destruct D as [x'']. eauto.


 Case "XCase".
  right.
  inverts keep H1.
  have (value x \/ (exists s' x', STEP s x s' x')) as HS.
  inverts HS. clear IHx.

  (* Discriminant is a value *)
  SCase "x value".
   destruct x; nope.

    (* When we have a well typed case match on some data object, 
       then there is a corresponding alternative. *)
    SSCase "XLoc".
     admit.
(*     have (exists x, getAlt dc aa = Some (AAlt d x)).
     dest x. exists (substXXs 0 l0 x).
     eapply EsCaseAlt; eauto.
*)

    (* Can't happen, 
       TForall has no data type constructor *)
    SSCase "XCase (XLAM x) aa".
     have (exists t', tObj = TForall t').
     dest t'. subst. false.

    (* Can't happen, 
       tFun has no data type constructor *)
    SSCase "XCase (XLam t x) aa".
     have (exists t11 t12, tObj = tFun t11 t12).
     dest t11. dest t12. subst.
     unfold tFun in H6. simpl in H6. inverts H6.
     inverts H.
     have (DEFOK ds (DefDataType TyConFun ks dcs)) as HD.
     inverts HD. false.

  (* Discriminant steps *)
  SCase "x steps".
   destruct H2 as [s'].
   destruct H2 as [x'].
   exists s'.
   exists (XCase x' aa).
   lets D: EsContext XcCase; eauto.

 Case "XUpdate".
  right.
  inverts_type.
  edestruct IHx1; eauto.
  SCase "value x1".
   edestruct IHx2; eauto.
   SSCase "value x2".
    assert (exists l, x1 = XLoc l).
     admit.                                                         (* TODO: forms of values *)
    dest l. subst.
    assert (exists dc svs, get l s = Some (SObj dc svs)) as HJ.
     admit.                                                         (* TODO: lemma *)
    destruct HJ as [dc'].
    destruct H2 as [svs].

    (* Case on whether the data constructor matches *)
    assert (dc' = dc \/ ~(dc' = dc)) as HM.
     admit. inverts HM.                                             (* TODO: lemma *)

     SSSCase "dc' = dc".
      assert (exists sv2, svalueOf x2 sv2).
       admit. dest sv2.                                             (* TODO: lemma *)
      exists (replace l (SObj dc (replace i sv2 svs)) s).
      exists xUnit.
      eapply EsUpdate; eauto.

     SSSCase "dc <> dc".
      exists s.
      exists xUnit.
      eapply EsUpdateSkip; eauto.

   SSCase "x2 steps".
    destruct H1 as [s'].
    destruct H1 as [x1'].
    lets D: EsContext XcUpdate2 H1; eauto.

   SSCase "x1 steps".
    destruct H0 as [s'].
    destruct H0 as [x1'].
    lets D: EsContext XcUpdate1 H0; eauto.

 Case "XAlt".
   auto.
Qed.


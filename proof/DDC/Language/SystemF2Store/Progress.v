
Require Import DDC.Language.SystemF2Store.Step.
Require Import DDC.Language.SystemF2Store.TyJudge.
Require Import DDC.Language.SystemF2Store.Exp.
Require Import DDC.Base.


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
     lets D: getAlt_has H H1.
     dest dc. int. 
     destruct H8  as [svs].
     destruct H10 as [x].
     exists s.
     assert (exists vs, Forall2 svalueOf vs svs).
      eapply Forall2_exists_left_from_right.
      eauto.
     dest vs.
     exists (substXXs 0 vs x).
     eauto.

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
     eapply value_loc; eauto.
     defok ds (DefDataType tcObj ks dcs). auto.
    dest l. subst.
    have (exists dc svs, get l s = Some (SObj dc svs)) as HJ.
    destruct HJ as [dc'].
    destruct H2 as [svs].

    (* Case on whether the data constructor matches *)
    have (dc' = dc \/ ~(dc' = dc)) as HM.
     inverts HM.

     SSSCase "dc' = dc".
      assert (exists sv2, svalueOf x2 sv2).
       eauto. dest sv2.
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


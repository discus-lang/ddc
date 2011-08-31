
Require Import DDC.Language.SystemF2Data.Step.
Require Import DDC.Language.SystemF2Data.TyJudge.
Require Import DDC.Language.SystemF2Data.Exp.
Require Import DDC.Base.


(* A well typed expression is either a well formed value, 
   or can transition to the next state. *)
Theorem progress
 :  forall ds x t
 ,  DEFSOK ds
 -> TYPE ds nil nil x t
 -> value x \/ (exists x', STEP x x').
Proof.
 intros. gen t.
 induction x using exp_mutind with 
  (PA := fun a => a = a)
  ; intros.

 Case "XVar".
  nope.

 Case "XLAM".
  left. apply type_wfX in H0. auto.

 Case "XAPP".
  inverts keep H0.
  edestruct IHx. eauto.
  SCase "x value".
   right. inverts H1. inverts H3.
    SSCase "x = XVar". nope.
    SSCase "x = XLAM". exists (substTX 0 t2 x1). eapply ESLAMAPP.
    SSCase "x = XAPP". nope.
    SSCase "x = XApp". nope.

    SSCase "x = XCon".
     (* TODO: XAPP (XCon dc tsParam xs) t2) cant happen as it's miskinded.
              need lemma saying kind of value types is KStar *)
     skip.

    SSCase "x = XCase".
     nope.
  SCase "x steps".
   right.
    dest x'.
    exists (XAPP x' t2).
    lets D: EsContext XcAPP H1.
    eauto.

 Case "XLam".
  left. eapply type_wfX in H0. auto.

 Case "XApp".
  right.
  inverts_type.
  edestruct IHx1; eauto.
  SCase "value x1".
   edestruct IHx2; eauto.
    SSCase "value x2".
     assert (exists t x, x1 = XLam t x) as HF. eauto.
     destruct HF as [t11].
     destruct H2 as [x12].
     subst.
     exists (substXX 0 x2 x12). 
     apply EsLamApp; eauto.
    SSCase "x2 steps".
     destruct H1 as [x2'].
     exists (XApp x1 x2'). auto.
  SCase "x1 steps".
   destruct H0  as [x1'].
   exists (XApp x1' x2).
   eapply (EsContext (fun xx => XApp xx x2)); auto.
 
 Case "XCon".
  inverts_type.
  (* All ctor args are either wnf or can step *)
  assert (Forall (fun x => wnfX x \/ (exists x', STEP x x')) xs) as HWS.
   nforall. intros.
   assert (exists t, TYPE ds nil nil x t).
    eapply Forall2_exists_left; eauto.
    dest t.
   assert (value x \/ (exists x', STEP x x')).
    eapply H0; eauto.
   int.     

  (* All ctor args are wnf, or there is a context where one can step *)
  lets D: (@exps_ctx_run exp exp) HWS.
  inverts D.
   (* All ctor args are wnf *)
   left.
   assert (Forall (wfT 0) ts).   eauto. admit. (* ok ts WK under nil*)
   assert (Forall (wfX 0 0) xs). eauto. admit. (* ok xs WT under nil nil *)
   eauto. 

   (* There is a context where one ctor arg can step *)
   right.
    dest C. dest x'.
    int. subst.
    lets D: step_context_XCon_exists H2 H4.
    destruct D as [x'']. eauto.

 Case "XCase".
  right.
  inverts keep H1.
  assert (value x \/ (exists x', STEP x x')) as HS; eauto.
  inverts HS. clear IHx.
  SCase "x value".
   destruct x; nope.
    SSCase "XCase (XLAM x) aa".
     assert (exists t', tObj = TForall t'). eauto.
     dest t'. subst.
     false.

    SSCase "XCase (XLam t x) aa".
     assert (exists t11 t12, tObj = tFun t11 t12). eauto.
     dest t11. dest t12. subst.
     unfold tFun in H6. 
     simpl in H6. inverts H6.
     assert (DEFOK ds (DefDataType TyConFun ks dcs)). eauto.
      inverts H3. false.

    SSCase "XCon".
     (* show there is a corresponding alternative 
        TODO: split this into a lemma *)
     assert (exists x, getAlt d aa = Some (AAlt d x)) as HG.
      eapply getAlt_exists.
      nforall.
      eapply H9. 
      inverts H4.

      assert (getCtorOfType (TCon tc) = Some tc). auto.
      erewrite getCtorOfType_makeTApps in H6; eauto.
       inverts H6.
      eauto.

     dest x.
     exists (substXXs 0 l0 x).
     eapply EsCaseAlt; eauto.

  SCase "x steps".
   destruct H2 as [x'].
   exists (XCase x' aa).
   lets D: EsContext XcCase; eauto.

 Case "XAlt".
   auto.     
Qed.



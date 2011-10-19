
Require Import DDC.Language.SimpleData.Step.
Require Import DDC.Language.SimpleData.TyJudge.
Require Import DDC.Language.SimpleData.Exp.
Require Import DDC.Base.


(* A well typed expression is either a well formed value, 
   or can transition to the next state. *)
Theorem progress
 :  forall ds x t
 ,  TYPE ds nil x t
 -> value x \/ (exists x', STEP x x').
Proof.
 intros. gen t.
 induction x using exp_mutind with 
  (PA := fun a => a = a)
  ; intros.
 
 Case "XVar".
  nope.

 Case "XLam".
  left. eauto.

 Case "XApp".
  right.
  inverts_type.
  edestruct IHx1; eauto.
  SCase "value x1".
   edestruct IHx2; eauto.
    SSCase "value x2".
     assert (exists t x, x1 = XLam t x) as HF. eauto.
     destruct HF as [t11].
     destruct H1 as [x12].
     subst.
     exists (substX 0 x2 x12). 
     apply EsLamApp; eauto.
    SSCase "x2 steps".
     destruct H0 as [x2'].
     exists (XApp x1 x2'). auto.
  SCase "x1 steps".
   destruct H  as [x1'].
   exists (XApp x1' x2).
   eapply (EsContext (fun xx => XApp xx x2)); auto.
 
 Case "XCon".
  inverts_type.
  (* All ctor args are either wnf or can step *)
  assert (Forall (fun x => wnfX x \/ (exists x', STEP x x')) xs) as HWS.
   nforall. intros.
   assert (exists t, TYPE ds nil x t).
    eapply Forall2_exists_left; eauto.
    dest t.
   assert (value x \/ (exists x', STEP x x')).
    eapply H; eauto.
   burn. 

  (* All ctor args are wnf, or there is a context where one can step *)
  lets D: (@exps_ctx_run exp exp) HWS.
  inverts D.
   (* All ctor args are wnf *)
   left. eauto 6.
   (* There is a context where one ctor arg can step *)
   right.
    dest C. dest x'.
    rip.
    lets D: step_context_XCon_exists H1 H5.
    destruct D as [x'']. eauto.

 Case "XCase".
  right.
  inverts_type.
  assert (value x \/ (exists x', STEP x x')) as HS; eauto.
  inverts HS. clear IHx.
  SCase "x value".
   destruct x; nope.
    SSCase "XCon".
     inverts_type.
     assert (dcs0 = dcs).
      rewrite H8 in H12. inverts H12. auto. subst.
     assert (exists ts x, getAlt d aa = Some (AAlt d ts x)) as HG.
      eapply getAlt_exists.
      nforall. eauto.
     dest ts. dest x.
     exists (substXs 0 l x).
     eapply EsCaseAlt; eauto.

  SCase "x steps".
   destruct H0 as [x'].
   exists (XCase x' aa).
   lets D: EsContext XcCase; eauto.

 Case "XAlt".
   auto.     
Qed.


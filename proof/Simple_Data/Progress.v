
Require Import EsJudge.
Require Import TyJudge.
Require Import Exp.
Require Import Base.


(* A well typed expression is either a well formed value, 
   or can transition to the next state.
 *)
Theorem progress
 :  forall ds t T
 ,  TYPE ds Empty t T
 -> value t \/ (exists t', STEP t t').
Proof.
 intros.
 remember (@Empty ty) as tenv.
 induction H.

 Case "XVar".
  subst. inverts H.

 Case "XLam".
  left. subst. eauto. 

 Case "XApp".
  right. 
  specializes IHTYPE1 Heqtenv.
  specializes IHTYPE2 Heqtenv.
  destruct IHTYPE1.

  SCase "value x1".
   destruct IHTYPE2.

   SSCase "value x2".
    inverts H1. inverts H3.

     SSSCase "x1 ~ XVar".
      inverts H4. destruct H5. false.

     SSSCase "x1 ~ XLam".
      exists (substX 0 x2 x0).
      apply EsLamApp.
      inverts H2. auto.

     SSSCase "x1 ~ XCon".
      inverts H. admit. (*** add to TyCon that result must be data type *)

   SSCase "x2 steps".
    destruct H2 as [x2'].
    exists (XApp x1 x2'). eauto.

   SSCase "x1 steps".
    destruct H1 as [x1'].
    exists (XApp x1' x2).
    eapply (EsContext (fun xx => XApp xx x2)); eauto.

 Case "XCon".
  admit. (* finish this *)

 Case "XCase".
  subst.
  assert (@Empty ty = Empty). auto.
   lets D: IHTYPE H4. clear IHTYPE. clear H4.
  inverts D.

  SCase "xObj value".
   right.
   destruct xObj.
    SSCase "XVar". inverts H4. inverts H6. destruct H8. false.
    SSCase "XLam". inverts H.
    SSCase "XApp". inverts H4. inverts H5.
    SSCase "XCon".
     rewrite Forall_forall in H0.
     assert (exists tsArgs x, getAlt d alts = Some (AAlt d tsArgs x)).
      admit.
     destruct H5. destruct H5.

     exists (substXs 0 l x0).
     eapply EsCaseAlt.
      admit. (* ok. all con args are values, from value (XCon d l) *)
      eauto.

    SSCase "XCase". inverts H4. inverts H5.   
    
  SCase "xObj steps".
   right.
   destruct H4 as [xObj'].
   exists (XCase xObj' alts).
    eapply (EsContext (fun xx => XCase xx alts)).
    admit. (***** TODO:  we're missing the case context *)
    auto.
Qed.



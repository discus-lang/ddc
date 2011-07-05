
Require Export DDC.Language.SimpleData.TyJudge.
Require Export DDC.Language.SimpleData.Exp.
Require Export DDC.Language.SimpleData.SubstExpExp.



Lemma getAlt_good
 :  forall ds te tPat tResult alts dc tsArgs x
 ,  Forall (fun a => TYPEA ds te a tPat tResult) alts
 -> In (AAlt dc tsArgs x) alts
 -> TYPE ds (te >< tsArgs) x tResult.
Proof.
 intros.
 rewrite Forall_forall in H.
 eapply H in H0. inverts H0. eauto.
Qed.


Lemma getAlt_inAlts
 :  forall ds te tResult tCon alts dc tsArgs x1 x2
 ,  TYPE ds te (XCase x1 alts) tResult
 -> getDataDef dc ds   = Some (DefData dc tsArgs tCon)
 -> getAlt     dc alts = Some (AAlt    dc tsArgs x2)
 -> TYPE ds (te >< tsArgs) x2 tResult.
Proof.
 intros.
 inverts keep H.
 lets HA:  getAlt_in H1.
 lets HXT: getAlt_good H5 HA.
 auto.
Qed.
 

Lemma getAlt_matches_dataDef
 :  forall ds te tCon tResult alts dc x tsArgs tsArgs'
 ,  Forall (fun alt => TYPEA ds te alt tCon tResult) alts 
 -> getDataDef dc ds = Some (DefData dc tsArgs  tCon)
 -> getAlt dc alts   = Some (AAlt    dc tsArgs' x)
 -> tsArgs = tsArgs'.
Proof.
 intros.
 rewrite Forall_forall in H.
 lets D: getAlt_in H1.
 apply H in D.
 inverts D.
 rewrite H0 in H9.
 inverts H9. auto.
Qed.



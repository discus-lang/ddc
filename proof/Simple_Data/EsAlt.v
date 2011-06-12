
Require Export TyJudge.
Require Export Exp.
Require Export SubstExpExp.

(* Get the alternative body that matches a given constructor. *)
Fixpoint getAlt (dc: datacon) (alts: list alt) {struct alts} 
                : option alt :=
 match alts with 
 |  nil  => None

 |  AAlt dc' tsArgs x :: alts'
 => if datacon_beq dc dc'
     then Some (AAlt dc' tsArgs x)
     else getAlt dc alts'
 end.


Lemma getAltExp_hasAlt
 :  forall dc alt alts
 ,  getAlt dc alts = Some alt
 -> In alt alts.
Proof.
 intros.
 induction alts.
  false.
  destruct a as [dc' tsArgs x].
  simpl in H.
  breaka (datacon_beq dc dc').
   inverts H.
    apply datacon_beq_eq in HeqX. subst.
    simpl. auto.
   simpl. right. auto.
Qed.


Lemma getAltExp_good
 :  forall ds te tPat tResult alts dc tsArgs x
 ,  Forall (fun a => TYPEA ds te a tPat tResult) alts
 -> In (AAlt dc tsArgs x) alts
 -> TYPE ds (te ++ envOfList tsArgs) x tResult.
Proof.
 intros.
 rewrite Forall_forall in H.
 eapply H in H0. inverts H0. eauto.
Qed.


Lemma getAltExp_inAlts
 :  forall ds te tResult tCon alts dc tsArgs x1 x2
 ,  TYPE ds te (XCase x1 alts) tResult
 -> getDataDef dc ds   = Some (DefData dc tsArgs tCon)
 -> getAlt     dc alts = Some (AAlt    dc tsArgs x2)
 -> TYPE ds (te ++ envOfList tsArgs) x2 tResult.
Proof.
 intros.
 inverts keep H.
 lets HA:  getAltExp_hasAlt H1.
 lets HXT: getAltExp_good H5 HA.
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
 lets D: getAltExp_hasAlt H1.
 apply H in D.
 inverts D.
 rewrite H0 in H9.
 inverts H9. auto.
Qed.

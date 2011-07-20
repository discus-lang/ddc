
Require Export DDC.Language.SimpleData.TyJudge.
Require Export DDC.Language.SimpleData.Exp.
Require Export DDC.Language.SimpleData.SubstExpExp.


(* The body of an alternative is well typed under an environment
   that includes the arguments of the constructor being matched. *)
Lemma getAlt_bodyIsWellTyped_fromAlts
 :  forall ds te tPat tResult alts dc tsArgs x
 ,  Forall (fun a => TYPEA ds te a tPat tResult) alts
 -> In (AAlt dc tsArgs x) alts
 -> TYPE ds (te >< tsArgs) x tResult.
Proof.
 intros.
 nforall. spec H H0. inverts_type. auto.
Qed.


(* The body of an alternative of a case expression is well typed
   under an environment that includes the arguments of the constructor
   being matched. *)
Lemma getAlt_bodyIsWellTyped_fromCase
 :  forall ds te tResult tCon alts dc tsArgs x1 x2
 ,  TYPE ds te (XCase x1 alts) tResult
 -> getDataDef dc ds   = Some (DefData dc tsArgs tCon)
 -> getAlt     dc alts = Some (AAlt    dc tsArgs x2)
 -> TYPE ds (te >< tsArgs) x2 tResult.
Proof.
 intros.
 inverts keep H.
 lets HA:  getAlt_in H1.
 lets HXT: getAlt_bodyIsWellTyped_fromAlts H5 HA.
 auto.
Qed.
 

(* If an alternative is well typed then the types of the ctor
   args embedded in it match those in the data type definition *)
Lemma getAlt_ctorArgTypesMatchDataDef
 :  forall ds te tCon tResult alts dc x tsArgs tsArgs'
 ,  Forall (fun alt => TYPEA ds te alt tCon tResult) alts 
 -> getDataDef dc ds = Some (DefData dc tsArgs  tCon)
 -> getAlt dc alts   = Some (AAlt    dc tsArgs' x)
 -> tsArgs = tsArgs'.
Proof.
 intros.
 nforall.
 lets D: getAlt_in H1. spec H D.
 inverts_type.
 rewrite H0 in H9.
 inverts H9. auto.
Qed.



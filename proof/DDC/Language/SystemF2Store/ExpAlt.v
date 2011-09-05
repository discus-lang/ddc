
Require Export DDC.Language.SystemF2Store.ExpBase.
Require Export DDC.Base.


(* Get the data constructor of an alternative. *)
Fixpoint dcOfAlt (aa: alt) : datacon :=
 match aa with 
 | AAlt dc _ => dc
 end.
Hint Unfold dcOfAlt.


(* Get the alternative body that matches a given constructor. *)
Fixpoint getAlt (dc: datacon) (alts: list alt) {struct alts} 
                : option alt :=
 match alts with 
 |  nil  => None

 |  AAlt dc' x :: alts'
 => if datacon_beq dc dc'
     then Some (AAlt dc' x)
     else getAlt dc alts'
 end.


(* If we get a single alternative from a list, 
   then that alternative was in the list. *)
Lemma getAlt_in
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
Hint Resolve getAlt_in.


(* Given a data constructor, if one of the alternatives in a 
   list matches that data constructor then we can get the other
   information about the alternative from the list. *)
Lemma getAlt_exists
 :  forall d alts
 ,  In d (map dcOfAlt alts)
 -> (exists x, getAlt d alts = Some (AAlt d x)).
Proof.
 intros.
 induction alts.
  simpl in H. false.
  simpl in H. inverts H.
   destruct a. simpl.
   breaka (datacon_beq d d).
    exists e. auto.
    apply datacon_beq_false in HeqX. false.
   lets D: IHalts H0.
   destruct a. simpl.
    breaka (datacon_beq d d0).
     apply datacon_beq_eq in HeqX. subst. auto.
     exists e. auto.
Qed.
Hint Resolve getAlt_exists.


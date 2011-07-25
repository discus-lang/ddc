
Require Import DDC.Language.SystemF2.Ty.
Require Import DDC.Base.


(* Data Constructors *)
Inductive datacon : Type :=
 | DataCon    : nat -> datacon.
Hint Constructors datacon.


(* Definitions. 
   Carries meta information about type and data constructors. *)
Inductive def  : Type :=
 (* Definition of a data type constructor *)
 | DefDataType 
   :  tycon        (* Name of data type constructor *)
   -> list datacon (* Data constructors that belong to this type *)
   -> def

 (* Definition of a data constructor *)
 | DefData 
   :  datacon      (* Name of data constructor *)
   -> list ty      (* Types of arguments *)
   -> ty           (* Type  of constructed data *)
   -> def.
Hint Constructors def.


(* Definition environment.
   Holds the definitions of all current type and data constructors. *)
Definition defs  := list def.


(* Lookup the def of a given type constructor.
   Returns None if it's not in the list. *)
Fixpoint getTypeDef (tc: tycon) (ds: defs) : option def := 
 match ds with 
 | ds' :> DefDataType tc' _ as d
 => if tycon_beq tc tc' 
     then  Some d
     else  getTypeDef tc ds'

 | ds' :> _ => getTypeDef tc ds'
 | Empty    => None
 end.


(* Lookup the def of a given data constructor. 
   Returns None if it's not in the list. *)
Fixpoint getDataDef (dc: datacon) (ds: defs) : option def := 
 match ds with 
 | ds' :> DefData dc' _ _ as d
 => if datacon_beq dc dc' 
     then  Some d
     else  getDataDef dc ds'

 | ds' :> _ => getDataDef dc ds'
 | Empty    => None
 end.


(* Boolean equality for data constructors. *)
Lemma datacon_beq_eq
 :  forall dc dc' 
 ,  true = datacon_beq dc dc'
 -> dc = dc'.
Proof.
 intros.
 destruct dc.
 destruct dc'.
 simpl in H.
 apply beq_nat_eq in H.
 auto.
Qed.


(* Boolean negation for data constructors. *)
Lemma datacon_beq_false
 :  forall dc 
 ,  false = datacon_beq dc dc 
 -> False.
Proof.
 intro.
 destruct dc.
 simpl.
 intros.
  induction n.
  simpl in H. false.
  simpl in H. auto.
Qed.


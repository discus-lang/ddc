
Require Import DDC.Language.SystemF2.Ki.
Require Import DDC.Language.SystemF2.KiJudge.
Require Import DDC.Language.SystemF2.Ty.
Require Import DDC.Base.
Require Import Coq.Bool.Bool.

(********************************************************************)
(* Data Constructors
   Carries a data constructor tag and an arity. *)
Inductive datacon : Type :=
 | DataCon    : nat -> nat -> datacon.
Hint Constructors datacon.


Lemma beq_true_and_split
 :  forall a1 a2
 ,  true = a1 && a2
 -> true = a1 /\ true = a2.
Proof.
 destruct a1; destruct a2; auto.
Qed.


Lemma beq_false_and_split
 :  forall a1 a2
 ,  false = a1 && a2
 -> false = a1 \/ false = a2.
Proof.
 destruct a1; destruct a2; auto. 
Qed.


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
  apply beq_true_and_split in H. inverts H.
  apply beq_nat_eq in H0.
  apply beq_nat_eq in H1.
  burn.
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
  apply beq_false_and_split in H.
  inverts H.
   induction n. false. auto.
   induction n0. false. auto.
Qed.


(********************************************************************)
(* Definitions. 
   Carries meta information about type and data constructors. *)
Inductive def  : Type :=
 (* Definition of a data type constructor *)
 | DefDataType 
   :  tycon        (* Name of data type constructor *)
   -> list ki      (* Kinds of type parameters *)
   -> list datacon (* Data constructors that belong to this type *)
   -> def

 (* Definition of a data constructor *)
 | DefData 
   :  datacon      (* Name of data constructor *)
   -> list ty      (* Types of arguments *)
   -> tycon        (* Type constructor of constructed data *)
   -> def.
Hint Constructors def.


(* Definition environment.
   Holds the definitions of all current type and data constructors. *)
Definition defs  := list def.


(* Lookup the def of a given type constructor.
   Returns None if it's not in the list. *)
Fixpoint getTypeDef (tc: tycon) (ds: defs) : option def := 
 match ds with 
 | ds' :> DefDataType tc' _ _ as d
 => if tycon_beq tc tc' 
     then  Some d
     else  getTypeDef tc ds'

 | ds' :> _ => getTypeDef tc ds'
 | Empty    => None
 end.

Lemma getTypeDef_in
 :  forall tc ds ddef
 ,  getTypeDef tc ds = Some ddef
 -> In ddef ds.
Proof.
 intros.
 induction ds.
  false.
  destruct a.
   simpl in H.
   destruct (tycon_beq tc t). 
    inverts H. int.
   int. int.
Qed.


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

Lemma getDataDef_in
 :  forall tc ds ddef
 ,  getDataDef tc ds = Some ddef
 -> In ddef ds.
Proof.
 intros.
 induction ds.
  false.
  destruct a.
   int.
   simpl in H.
   destruct (datacon_beq tc d). 
    inverts H. int.
   int.
Qed.


(********************************************************************)
(* Check that a definition is ok. *)
Inductive DEFOK : list def -> def -> Prop :=
 (* Check that a data type definition is ok *)
 | DefOkType
   :  forall ds tc ks dcs
      (* There must be at least one data constructor *)
   ,  length dcs > 0
      
      (* All the data constructors must be present in the list of defs *)
   -> Forall (fun dc => exists ddef, getDataDef dc ds = Some ddef) dcs

   -> DEFOK ds (DefDataType tc ks dcs)

 (* Check that a data constructor definition is ok. *)
 | DefOkData
   :  forall tc ds ks dc dcs tsArgs
      (* Get the data type def this data ctor belongs to *)
   ,  getTypeDef tc ds = Some (DefDataType tc ks dcs)

      (* Type constructor must be a data type constructor, 
         not the function type constructor. *)
   -> isTyConData tc

      (* Data ctor must be one of the ctors in the data type def *)
   -> In dc dcs

      (* All the ctor arg types must be well kinded in an environment
         consistin of just the parameter types. *)
   -> Forall (fun t => KIND ks t KStar) tsArgs

   -> DEFOK ds (DefData dc tsArgs tc).


(********************************************************************)
(* Check that some data type definitions and their
   associated constructors are ok *)
Definition DEFSOK (ds: list def) : Prop :=
  Forall (DEFOK ds) ds.

Lemma getTypeDef_ok 
 :  forall ds tc ddef
 ,  DEFSOK ds
 -> getTypeDef tc ds = Some ddef
 -> DEFOK  ds ddef.
Proof.
 intros.
 unfold DEFSOK in H.
 apply getTypeDef_in in H0.
 nforall. auto.
Qed.  

Lemma getDataDef_ok
 :  forall ds tc ddef
 ,  DEFSOK ds
 -> getDataDef tc ds = Some ddef
 -> DEFOK ds ddef.
Proof.
 intros.
 unfold DEFSOK in H.
 apply getDataDef_in in H0.
 nforall. auto.
Qed.  


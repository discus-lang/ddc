
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


Fixpoint datacon_beq t1 t2 :=
  match t1, t2 with
  | DataCon n11 n12, DataCon n21 n22 
  => beq_nat n11 n21 && beq_nat n12 n22
  end.

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


Lemma datacon_eq_ex
 : forall (dc1 dc2 : datacon)
 , dc1 = dc2 \/ dc1 <> dc2.
Proof.
 intros.
 assert (exists b, b = datacon_beq dc1 dc2).
  eauto. dest b.
 destruct b. 
  apply datacon_beq_eq in H. subst. auto.
  right. unfold not. intros.
  subst. apply datacon_beq_false in H. auto.
Qed.
Hint Resolve datacon_eq_ex.


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
Hint Resolve getTypeDef_in.


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
Hint Resolve getDataDef_in.




(********************************************************************)
(* Check that a definition is ok. *)
Inductive DEFOK : list def -> def -> Prop :=
 (* Check that a data type definition is ok *)
 | DefOkType
   :  forall ds tc ks dcs
      (* Type constructor must be a data type constructor, 
         not the function type constructor. *)
   ,  isTyConData tc

      (* There must be at least one data constructor *)
   -> length dcs > 0
      
      (* All the data constructors must be present in the list of defs *)
   -> Forall (fun dc => exists ddef, getDataDef dc ds = Some ddef) dcs

   -> DEFOK ds (DefDataType tc ks dcs)

 (* Check that a data constructor definition is ok. *)
 | DefOkData
   :  forall tc ds ks dcs tsArgs tag arity
      (* Type constructor must be a data type constructor, 
         not the function type constructor. *)
   ,  isTyConData tc

      (* Get the data type def this data ctor belongs to *)
   -> getTypeDef tc ds = Some (DefDataType tc ks dcs)

      (* Data ctor must be one of the ctors in the data type def *)
   -> In (DataCon tag arity) dcs

   -> length tsArgs = arity

      (* All the ctor arg types must be well kinded in an environment
         consistin of just the parameter types. *)
   -> Forall (fun t => KIND ks t KStar) tsArgs

   -> DEFOK ds (DefData (DataCon tag arity) tsArgs tc).


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
Hint Resolve getTypeDef_ok.


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
Hint Resolve getDataDef_ok.


Lemma getDataDef_datacon_in
 :  forall d ds ts tc ks dcs
 ,  DEFSOK ds
 -> getDataDef d  ds = Some (DefData     d  ts tc)
 -> getTypeDef tc ds = Some (DefDataType tc ks dcs)
 -> In d dcs.
Proof.
 intros.
 assert (DEFOK ds (DefData d ts tc)) as HD. eauto.
 assert (DEFOK ds (DefDataType tc ks dcs)) as HT. eauto.
 inverts HD. inverts HT.
 rewrite H1 in H6. inverts H6.
 auto.
Qed.
Hint Resolve getDataDef_datacon_in.


(******************************************************************************)
(* Tactic to help unpack data definitions *)
Ltac ddef_merge
 := match goal with 
    | [ H1 : getTypeDef ?tc ?ds = Some (DefDataType _ ?ks0 ?dcs0)
      , H2 : getTypeDef ?tc ?ts = Some (DefDataType _ ?ks1 ?dcs1) |- _ ]
    => assert (ks1 = ks0 /\ dcs1 = dcs0) as HA
         by (rewrite H1 in H2; inverts H2; auto);
       inverts HA; clear H2

    | [ H1 : getDataDef ?dc ?ds = Some (DefData _ ?dc0 ?ts0)
      , H2 : getDataDef ?dc ?ts = Some (DefData _ ?dc1 ?ts1) |- _ ]
    => assert (dc1 = dc0 /\ ts1 = ts0) as HA
         by (rewrite H1 in H2; inverts H2; auto);
       inverts HA; clear H2
    end.


Tactic Notation "defok" constr(ds) constr(ddef)
 := let H := fresh
    in assert (DEFOK ds ddef) as H by burn;
       inverts H; try ddef_merge.

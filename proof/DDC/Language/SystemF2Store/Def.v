
Require Import DDC.Language.SystemF2.Ki.
Require Import DDC.Language.SystemF2.KiJudge.
Require Import DDC.Language.SystemF2.Ty.
Require Import DDC.Language.SystemF2Store.ExpDataCon.
Require Import DDC.Base.
Require Import Coq.Bool.Bool.


(********************************************************************) 
(* Definitions. 
   Carries meta information about type and data constructors. *)
Inductive def  : Type :=
 (* Definition of a data type constructor *)
 | DefType
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


(********************************************************************)
(* Lookup the def of a given type constructor.
   Returns None if it's not in the list. *)
Fixpoint getTypeDef (tc: tycon) (ds: defs) : option def := 
 match ds with 
 | ds' :> DefType tc' _ _ as d
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
    inverts H. burn. burn. burn.
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
   burn.
   simpl in H.
   destruct (datacon_beq tc d). 
    inverts H. burn. burn. 
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

   -> DEFOK ds (DefType tc ks dcs)

 (* Check that a data constructor definition is ok. *)
 | DefOkData
   :  forall tc ds ks dcs tsArgs tag arity
      (* Type constructor must be a data type constructor, 
         not the function type constructor. *)
   ,  isTyConData tc

      (* Get the data type def this data ctor belongs to *)
   -> getTypeDef tc ds = Some (DefType tc ks dcs)

      (* Data ctor must be one of the ctors in the data type def *)
   -> In (DataCon tag arity) dcs

   -> length tsArgs = arity

      (* All the ctor arg types must be well kinded in an environment
         consistin of just the parameter types. *)
   -> Forall (fun t => KIND ks t KStar) tsArgs

   -> DEFOK ds (DefData (DataCon tag arity) tsArgs tc).


(********************************************************************)
(* Builtin data types must be present in the list of definitions. *)
Definition dcUnit := DataCon   0 0. 
Hint Unfold dcUnit.

Definition tcUnit := TyConData 0 KStar.
Hint Unfold tcUnit.


(* Check that data type and data constructor definitions are
   well formed, and that the list of definitions contains the
   builtin types. *)   
Definition DEFSOK (ds: list def) : Prop 
 := Forall (DEFOK ds) ds
 /\ getTypeDef tcUnit ds = Some (DefType tcUnit nil (dcUnit :: nil))
 /\ getDataDef dcUnit ds = Some (DefData dcUnit nil tcUnit).

Lemma getTypeDef_ok 
 :  forall ds tc ddef
 ,  DEFSOK ds
 -> getTypeDef tc ds = Some ddef
 -> DEFOK  ds ddef.
Proof.
 intros.
 unfold DEFSOK in *.
 apply getTypeDef_in in H0.
 rip. nforall. auto. 
Qed.  
Hint Resolve getTypeDef_ok.


Lemma getDataDef_ok
 :  forall ds tc ddef
 ,  DEFSOK ds
 -> getDataDef tc ds = Some ddef
 -> DEFOK ds ddef.
Proof.
 intros.
 unfold DEFSOK in *.
 apply getDataDef_in in H0.
 rip. nforall. auto.
Qed.
Hint Resolve getDataDef_ok.


Lemma getDataDef_datacon_in
 :  forall d ds ts tc ks dcs
 ,  DEFSOK ds
 -> getDataDef d  ds = Some (DefData d  ts tc)
 -> getTypeDef tc ds = Some (DefType tc ks dcs)
 -> In d dcs.
Proof.
 intros.
 assert (DEFOK ds (DefData d ts tc)) as HD. eauto.
 assert (DEFOK ds (DefType tc ks dcs)) as HT. eauto.
 inverts HD. inverts HT.
 rewrite H1 in H6. inverts H6.
 auto.
Qed.
Hint Resolve getDataDef_datacon_in.


(******************************************************************************)
(* Tactic to help unpack data definitions *)
Ltac ddef_merge
 := repeat (match goal with 
    | [ H1 : getTypeDef ?tc ?ds = Some (DefType _ ?ks0 ?dcs0)
      , H2 : getTypeDef ?tc ?ts = Some (DefType _ ?ks1 ?dcs1) |- _ ]
    => let HA := fresh
       in  assert (ks1 = ks0 /\ dcs1 = dcs0) as HA
             by (rewrite H1 in H2; inverts H2; auto);
             inverts HA; clear H2

    | [ H1 : getDataDef ?dc ?ds = Some (DefData _ ?dc0 ?ts0)
      , H2 : getDataDef ?dc ?ts = Some (DefData _ ?dc1 ?ts1) |- _ ]
    => let HA := fresh
       in  assert (dc1 = dc0 /\ ts1 = ts0) as HA
             by (rewrite H1 in H2; inverts H2; auto);
             inverts HA; clear H2

    | [ H1 : get ?l ?se = Some (makeTApps (TCon ?tc0) ?ts0)
      , H2 : get ?l ?se = Some (makeTApps (TCon ?tc1) ?ts1) |- _ ]
    => let HA := fresh
       in  assert (tc1 = tc0 /\ ts1 = ts0) as HA
            by (rewrite H1 in H2; inverts H2; eapply makeTApps_eq_params; auto);
            inverts HA; clear H2 
    end).


Tactic Notation "defok" constr(ds) constr(ddef)
 := let H := fresh
    in assert (DEFOK ds ddef) as H by burn;
       inverts H; try ddef_merge.

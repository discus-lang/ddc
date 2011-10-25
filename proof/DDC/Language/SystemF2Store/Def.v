
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


(* Check whether two definitions match on their keys *)
Definition def_match (d1 : def) (d2 : def) : Prop
 := match d1, d2 with
    | DefType tc1 _ _, DefType tc2 _ _ => tc1 = tc2
    | DefData dc1 _ _, DefData dc2 _ _ => dc1 = dc2
    | _, _                             => False
    end.


(* For every pair of entries in the definitions list, 
   if their keys are equal then the entries are equal. *)
Definition defs_normal (ds: defs) : Prop
 := ForallPairs 
     (fun d1 d2 => def_match d1 d2 -> d1 = d2)
     ds.
Hint Unfold defs_normal.


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
   -> Forall (fun dc => exists ts, In (DefData dc ts tc) ds) dcs

   -> DEFOK ds (DefType tc ks dcs)

 (* Check that a data constructor definition is ok. *)
 | DefOkData
   :  forall tc ds ks dcs tsArgs tag arity
      (* Type constructor must be a data type constructor, 
         not the function type constructor. *)
   ,  isTyConData tc

      (* Get the data type def this data ctor belongs to *)
   -> In (DefType tc ks dcs) ds

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
 /\ defs_normal ds
 /\ In (DefType tcUnit nil (dcUnit :: nil)) ds
 /\ In (DefData dcUnit nil tcUnit) ds.
Hint Unfold DEFSOK.


(********************************************************************)
(* A sugared way to state that some def is in the environment *)
Definition hasDef (ds: defs) (d : def)
 := In d ds /\ DEFSOK ds.
Hint Unfold DEFSOK.


Lemma hasDef_in
 : forall d ds, hasDef ds d -> In d ds.
Proof. burn. Qed.
Hint Resolve hasDef_in.


Lemma hasDef_defsok
 : forall d ds, hasDef ds d -> DEFSOK ds.
Proof. burn. Qed.
Hint Resolve hasDef_defsok.


Lemma hasDef_defok
 : forall d ds, hasDef ds d -> DEFOK ds d.
Proof.
 intros.
 inverts H.
 unfold DEFSOK in *.
 rip. nforall. burn.
Qed.
Hint Resolve hasDef_defok.


Lemma hasDef_type_eq_in
 :  forall ds tc ks1 ks2 dcs1 dcs2
 ,  DEFSOK ds
 -> In (DefType tc ks1 dcs1) ds
 -> In (DefType tc ks2 dcs2) ds
 -> ks2 = ks1 /\ dcs2 = dcs1.
Proof.
 intros.
 unfold DEFSOK in *.
 unfold defs_normal in *.
 unfold ForallPairs in *.

 assert (ks2 = ks1 /\ dcs2 = dcs1).
  rip;
   have (DefType tc ks2 dcs2 = DefType tc ks1 dcs1)
     by (eapply H; eauto; burn).
  congruence.
  congruence.
 auto.
Qed.
Hint Resolve hasDef_type_eq_in.


Lemma hasDef_data_eq_in
 :  forall ds dc ts1 ts2 tc1 tc2
 ,  DEFSOK ds
 -> In (DefData dc ts1 tc1) ds
 -> In (DefData dc ts2 tc2) ds
 -> ts2 = ts1 /\ tc2 = tc1.
Proof.
 intros.
 unfold DEFSOK in *.
 unfold defs_normal in *.
 unfold ForallPairs in *.

 assert (ts2 = ts1 /\ tc2 = tc1).
  rip;
   have (DefData dc ts2 tc2 = DefData dc ts1 tc1)
     by (eapply H; eauto; burn).
  congruence.
  congruence.
 auto.
Qed.
Hint Resolve hasDef_data_eq_in.


Lemma hasDef_datacon_in
 :  forall ds dc ts tc ks dcs
 ,  hasDef ds (DefData dc ts tc)
 -> hasDef ds (DefType tc ks dcs)
 -> In dc dcs.
Proof.
 intros.
 have (DEFOK ds (DefData dc ts tc))  as HD. inverts HD.
 have (DEFOK ds (DefType tc ks dcs)) as HT. inverts HT.

 assert (dcs0 = dcs).
  eauto.
  unfold hasDef in *.  rip.
  unfold DEFSOK in H2. rip.
  unfold defs_normal in *.
  unfold ForallPairs in *.
  have (DefType tc ks dcs = DefType tc ks0 dcs0)
    by (eapply H2; eauto; burn).
  congruence.
  subst.
 
 eauto.
Qed.
Hint Resolve hasDef_datacon_in.


(******************************************************************************)
(* Tactic to help unpack data definitions *)
Ltac defs_merge
 := repeat (match goal with 

    | [ H1 : hasDef ?ds (DefType ?tc ?ks0 ?dcs0)
      , H2 : hasDef ?ds (DefType ?tc ?ks1 ?dcs1) |- _ ]
    => let HA := fresh
       in  assert (ks1 = ks0 /\ dcs1 = dcs0) as HA
             by (eapply hasDef_type_eq_in; eauto);
             inverts HA; clear H2

    | [ H1 : hasDef ?ds (DefData ?dc ?ts0 ?tc0)
      , H2 : hasDef ?ds (DefData ?dc ?ts1 ?tc1) |- _ ]
    => let HA := fresh
       in  assert (ts1 = ts0 /\ tc1 = tc0) as HA
             by (eapply hasDef_data_eq_in; eauto);
             inverts HA; clear H2

    | [ H1 : get ?l ?se = Some (makeTApps (TCon ?tc0) ?ts0)
      , H2 : get ?l ?se = Some (makeTApps (TCon ?tc1) ?ts1) |- _ ]
    => let HA := fresh
       in  assert (tc1 = tc0 /\ ts1 = ts0) as HA
            by (rewrite H1 in H2; inverts H2; eapply makeTApps_eq_params; auto);
            inverts HA; clear H2 
     end).

(*
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
*)


(******************************************************************************)
Lemma hasDef_wfT_tField
 :  forall ds tc ks dc dcs tField tsFields
 ,  hasDef ds (DefType tc ks dcs)
 -> hasDef ds (DefData dc tsFields tc)
 -> In tField tsFields
 -> wfT (length ks) tField.
Proof.
 intros.
 eapply kind_wfT.
 have (DEFOK ds (DefData dc tsFields tc)) as HD.
 inverts HD.
 unfold hasDef in *.
 rip.
 defs_merge.
 have (ks = ks0 /\ dcs = dcs0).
  rip.
 nforall.
 eauto.
Qed.
Hint Resolve hasDef_wfT_tField.



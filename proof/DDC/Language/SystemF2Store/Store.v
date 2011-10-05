
Require Export DDC.Language.SystemF2Store.TyJudge.
Require Export DDC.Language.SystemF2Store.Exp.


(********************************************************************)
(* Storeable values are the ones that we can keep directly in 
   store bindings *)
Inductive svalue :=
 | SLoc  : nat -> svalue
 | SLAM  : exp -> svalue
 | SLam  : ty  -> exp -> svalue.
Hint Constructors svalue.


Definition takeSValueOfExp (xx : exp) : option svalue :=
 match xx with
 | XLoc n    => Some (SLoc n)
 | XLAM x    => Some (SLAM x)
 | XLam t x  => Some (SLam t x)
 | _         => None
 end.


Definition expOfSValue (s: svalue) : exp :=
 match s with
 | SLoc n    => XLoc n
 | SLAM x    => XLAM x
 | SLam t x  => XLam t x
 end.


Definition svalueOf (xx : exp) (sv : svalue) : Prop
 := takeSValueOfExp xx = Some sv.


(* There is an expression for every store value *)
Lemma exp_from_svalue
 : forall sv, exists v, svalueOf v sv.
Proof.
 intros.
 destruct sv; unfold svalueOf.
  exists (XLoc n). int.
  exists (XLAM e). int.
  exists (XLam t e). int.
Qed.
Hint Resolve exp_from_svalue.


(* There is a store value for every expression value. *)
Lemma svalue_from_value
 : forall v, value v -> (exists sv, svalueOf v sv).
Proof.
 intros.
 destruct v; nope; unfold svalueOf.
  exists (SLoc  n).  eauto.
  exists (SLAM  v).  eauto.
  exists (SLam t v). eauto.
Qed.
Hint Resolve svalue_from_value.


(******************************************************************************)
(* Store binding with a constructor tag and some storeable values *)
Inductive sbind :=
 | SObj : datacon -> list svalue -> sbind.


Definition store  := list sbind.
Hint Unfold store.


(******************************************************************************)
(* Store typing models the store.
   All types in the store typing have a corresponding binding in the store *)
Definition STOREM (ds: defs) (st: stenv) (s: store)
 := length st = length s.
Hint Unfold STOREM.


(* Well typed store. *)
Definition STORET (ds: defs) (st: stenv) (ss: store)
 := forall i dcObj svFields
 ,  get i ss = Some (SObj dcObj svFields)
 -> (exists tcObj tsParam tsFields 
    ,  get i st            = Some (makeTApps (TCon tcObj) tsParam)
    /\ getDataDef dcObj ds = Some (DefData dcObj tsFields tcObj)
    /\ Forall2 (TYPE ds nil nil st)
               (map expOfSValue svFields)
               (map (substTTs 0 tsParam) tsFields)).
Hint Unfold STORET.


(* Well formed store.
   Store is well formed under some data type definitions and a store typing. *)
Definition WfS (ds: defs) (se: stenv) (ss: store)
 := DEFSOK ds  
 /\ STOREM ds se ss 
 /\ STORET ds se ss.
Hint Unfold WfS.


Lemma store_has_sbind_for_stenv
 :  forall ds se ss l tObj
 ,  WfS ds se ss
 -> get l se = Some tObj
 -> (exists dc svs, get l ss = Some (SObj dc svs)).
Proof.
 intros.
 inverts H. int.
 have (length se = length ss).
 have (exists sb, get l ss = Some sb).
 dest sb.
 destruct sb.
 eauto.
Qed.
Hint Resolve store_has_sbind_for_stenv.


Lemma store_has_sbind_for_XLoc
 :  forall ds ke te se ss l tObj
 ,  WfS ds se ss
 -> TYPE ds ke te se (XLoc l) tObj
 -> (exists dc svs, get l ss = Some (SObj dc svs)).
Proof.
 intros.
 inverts H. int.
 inverts_type.
 eauto.
Qed.
Hint Resolve store_has_sbind_for_XLoc.




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


(********************************************************************)
(* Store binding with a constructor tag and some storeable values *)
Inductive sbind :=
 | SObj : datacon -> list svalue -> sbind.


Definition store  := list sbind.
Hint Unfold store.


(********************************************************************)
(* Store typing models the store.
   All types in the store typing have a corresponding binding in
   the store *)
Definition STOREM (ds: defs) (st: stenv) (s: store)
 := length st = length s.
Hint Unfold STOREM.


(********************************************************************)
(* Well typed store. *)
Definition STORET (ds: defs) (st: stenv) (ss: store)
 := forall l dcObj svFields
 ,  get l ss = Some (SObj dcObj svFields)
 -> (exists tcObj tsParam tsFields 
    ,  get l st            = Some (makeTApps (TCon tcObj) tsParam)
    /\ getDataDef dcObj ds = Some (DefData dcObj tsFields tcObj)
    /\ Forall2 (TYPE ds nil nil st)
               (map expOfSValue svFields)
               (map (substTTs 0 tsParam) tsFields)).
Hint Unfold STORET.


(* If we replace a field in a well typed store with one of the same
   type then the store is still well typed *)
Lemma storet_replace_field
 : forall ds se s tField vField1 svField1 vField2 svField2 svs i l dc
 ,  STORET ds se s
 -> TYPE ds nil nil se vField1 tField -> svalueOf vField1 svField1
 -> TYPE ds nil nil se vField2 tField -> svalueOf vField2 svField2
 -> get l s    = Some (SObj dc svs)
 -> get i svs  = Some svField1
 -> STORET ds se (replace l (SObj dc (replace i svField2 svs)) s).
Proof.
 intros.
 unfold STORET in *. rip.
 assert (l0 = l \/ l0 <> l) as HL. admit.
 destruct HL.
 Case "l0 = l".
  subst.
  spec H H4. dests H. 
  exists tcObj. exists tsParam. exists tsFields.
  rip.
  assert (dcObj = dc).                       (* ok, from get replace doesn't change dc *)
   admit. subst.
  auto.
  admit.                                     (* todo *)
 Case "l0 <> l".
  assert (dcObj = dc /\ svFields = svs). 
   admit. rip. subst.                        (* ok from get replace l0 <> l *)
  spec H l0 dc svs.
  assert (get l0 s = Some (SObj dc svs)).
   admit.                                    (* ok from get replace l0 <> l *)
  spec H H8.
  shifts H. eauto.
Qed.


(********************************************************************)
(* Well formed store.
   Store is well formed under some data type definitions and a
   store typing. *)
Definition WfS (ds: defs) (se: stenv) (ss: store)
 := DEFSOK ds
 /\ Forall closedT se
 /\ STOREM ds se ss 
 /\ STORET ds se ss.
Hint Unfold WfS.

Lemma WfS_defsok 
 : forall ds se ss, WfS ds se ss -> DEFSOK ds.
Proof. intros. inverts H. tauto. Qed.
Hint Resolve WfS_defsok.

Lemma WfS_closedT
 : forall ds se ss, WfS ds se ss -> Forall closedT se.
Proof. intros. inverts H. tauto. Qed.
Hint Resolve WfS_closedT.


(********************************************************************)
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
 inverts keep H. int.
 inverts_type.
 eauto.
Qed.
Hint Resolve store_has_sbind_for_XLoc.


(********************************************************************)
(* If we have a well typed case match on a store location containing
   some data object, then there is a case alternative corresponding to
   that object's data constructor. *)
Lemma getAlt_has
 :  forall ds se ss l alts t
 ,  WfS ds se ss
 -> TYPE ds nil nil se (XCase (XLoc l) alts) t
 -> (exists dc, (exists svs, get    l  ss   = Some (SObj dc svs))
            /\  (exists x,   getAlt dc alts = Some (AAlt dc x))).
Proof.
 intros.
 inverts H0.

 have (exists dc svs, get l ss = Some (SObj dc svs)).
  shift dc. split.
  shift svs. auto.
  dest svs.

 eapply getAlt_exists.
 inverts_type.
 nforall.
 inverts H. int.
 unfold STORET in *.
  spec H11 l dc svs H0.
  destruct H11 as [tcObj'].
  dest tsParam.
  dest tsFields.
  int.
  rewrite H2 in H11. inverts H11.
  erewrite getCtorOfType_makeTApps with (tc := tcObj') in H5; eauto.
    inverts H5.
  erewrite getCtorOfType_makeTApps with (tc := tcObj) in H7; eauto.
Qed.
Hint Resolve getAlt_has.



Require Export DDC.Language.SystemF2.Ki.
Require Export DDC.Language.SystemF2.KiJudge.
Require Export DDC.Language.SystemF2.TyEnv.
Require Export DDC.Language.SystemF2.TySubsts.
Require Export DDC.Language.SystemF2Store.Def.
Require Export DDC.Language.SystemF2Store.Exp.
Require Import Coq.Logic.FunctionalExtensionality.


(* Builtin in types. *)
Definition tUnit 
 := makeTApps (TCon tcUnit) nil.

Definition tFun (t1: ty) (t2: ty)
 := TApp (TApp (TCon TyConFun) t1) t2.
Hint Unfold tFun.


(* Store Typing holds the types of locations *)
Definition stenv := list ty.


(* Type Judgement assigns a type to an expression. *)
(* TODO: put DEFSOK at bottom of tree *)
Inductive TYPE (ds: defs) (ke: kienv) (te: tyenv) (se: stenv)
           : exp -> ty -> Prop :=

 (* Variables *)
 | TyVar 
   :  forall i t
   ,  get i te = Some t
   -> TYPE ds ke te se (XVar i) t

 (* Locations must refer to data types.
    Naked functions are not place in the store because
    they are not updatable and have no region annotations. *)
 | TyLoc
   :  forall i t tc
   ,  get i se         = Some t
   -> getCtorOfType t  = Some tc
   -> isTyConData tc

   -> TYPE ds ke te se (XLoc i) t

 (* Type abstraction *)
 | TyLAM 
   :  forall x1 t1
   ,  TYPE ds (ke :> KStar) (liftTE 0 te) (liftTE 0 se) x1 t1 
   -> TYPE ds ke             te           se            (XLAM x1) (TForall t1)

 (* Type application *)
 | TyAPP
   :  forall x1 t1 t2
   ,  TYPE ds ke te se x1 (TForall t1)
   -> KIND ke t2 KStar
   -> TYPE ds ke te se (XAPP x1 t2) (substTT 0 t2 t1)

 (* Function abstraction *)
 | TyLam
   :  forall x t1 t2
   ,  KIND ke t1 KStar
   -> TYPE ds ke (te :> t1) se x            t2
   -> TYPE ds ke te         se (XLam t1 x) (tFun t1 t2)

 (* Applications *)
 | TyApp
   :  forall x1 x2 t1 t2
   ,  TYPE ds ke te se x1           (tFun t1 t2)
   -> TYPE ds ke te se x2           t1
   -> TYPE ds ke te se (XApp x1 x2) t2

 (* Data Constructors *)
 | TyCon 
   :  forall tc (ks: list ki) tsFields tsParam xs dc dcs
   ,  DEFSOK ds
   -> getTypeDef tc ds = Some (DefType tc ks       dcs)
   -> getDataDef dc ds = Some (DefData dc tsFields tc)
   -> Forall2 (KIND ke) tsParam ks
   -> Forall2 (TYPE ds ke te se) xs (map (substTTs 0 tsParam) tsFields)
   -> TYPE ds ke te se (XCon dc tsParam xs) (makeTApps (TCon tc) tsParam)

 (* Case Expressions *)
 | TyCase
   :  forall xObj tObj tcObj ks tResult alts dcs

      (* check types of expression and alternatives *)
   ,  TYPE ds ke te se xObj tObj
   -> Forall (fun alt => TYPEA ds ke te se alt tObj tResult) alts

      (* All data cons must have a corresponding alternative. 
         Maybe we should move this to the well-formedness judgement *)
   -> getCtorOfType tObj  = Some tcObj
   -> getTypeDef tcObj ds = Some (DefType tcObj ks dcs)
   -> Forall (fun dc => In dc (map dcOfAlt alts)) dcs

   -> TYPE ds ke te se (XCase xObj alts) tResult

 (* Update data object *)
 | TyUpdate
   :  forall i xObj dcObj tcObj tsParam tsFields ks dcs xField tField
   ,  DEFSOK ds
   -> getTypeDef tcObj ds = Some (DefType tcObj ks dcs)
   -> getDataDef dcObj ds = Some (DefData dcObj tsFields tcObj)
   -> Forall2 (KIND ke) tsParam ks
   -> get i  tsFields     = Some tField
   -> TYPE ds ke te se xObj (makeTApps (TCon tcObj) tsParam) 
   -> TYPE ds ke te se xField (substTTs 0 tsParam tField)
   -> TYPE ds ke te se (XUpdate dcObj i tsParam xObj xField) tUnit

with TYPEA  (ds: defs) (ke: kienv) (te: tyenv) (se: stenv)
     : alt -> ty -> ty -> Prop :=
 (* Case Alternatives *)
 | TyAlt 
   :  forall x1 dc tc ks tsParam tsFields tResult dcs
   ,  DEFSOK ds
   -> getTypeDef tc ds = Some (DefType tc ks dcs)
   -> getDataDef dc ds = Some (DefData dc tsFields tc)
   -> Forall2 (KIND ke) tsParam ks
   -> TYPE  ds ke (te >< map (substTTs 0 tsParam) tsFields) se x1 tResult
   -> TYPEA ds ke te se (AAlt dc x1) (makeTApps (TCon tc) tsParam) tResult.

Hint Constructors TYPE.
Hint Constructors TYPEA.


(* Invert all hypothesis that are compound typing statements. *)
Ltac inverts_type :=
 repeat 
  (match goal with 
   | [ H: TYPE  _ _ _ _ (XVar  _)     _       |- _ ] => inverts H
   | [ H: TYPE  _ _ _ _ (XLoc  _)     _       |- _ ] => inverts H 
   | [ H: TYPE  _ _ _ _ (XLAM  _)     _       |- _ ] => inverts H
   | [ H: TYPE  _ _ _ _ (XAPP  _ _)   _       |- _ ] => inverts H
   | [ H: TYPE  _ _ _ _ (XLam  _ _)   _       |- _ ] => inverts H
   | [ H: TYPE  _ _ _ _ (XApp  _ _)   _       |- _ ] => inverts H
   | [ H: TYPE  _ _ _ _ (XCon  _ _ _) _       |- _ ] => inverts H
   | [ H: TYPE  _ _ _ _ (XCase _ _)   _       |- _ ] => inverts H
   | [ H: TYPE  _ _ _ _ (XUpdate _ _ _ _ _) _ |- _ ] => inverts H
   | [ H: TYPEA _ _ _ _ (AAlt _ _)    _ _     |- _ ] => inverts H
   end).


(********************************************************************)
(* Uniqueness of typing *)
Lemma type_unique
 :  forall ds ke te se x t1 t2 
 ,  DEFSOK ds
 -> TYPE ds ke te se x t1
 -> TYPE ds ke te se x t2
 -> t1 = t2.
Proof.
 intros. gen ds ke te se t1 t2.
 induction x using exp_mutind with 
  (PA := fun a => forall ds ke te se t2 t3 t3'
               ,  TYPEA ds ke te se a t2 t3
               -> TYPEA ds ke te se a t2 t3'
               -> t3 = t3');
  intros; try (solve [inverts_type; try congruence]).

 inverts_type. spec IHx H2 H3. auto. congruence.
 inverts_type. spec IHx H3 H4. auto. congruence.
 inverts_type. spec IHx H6 H7. auto. congruence.

 Case "XApp".
  inverts_type.
  spec IHx1 H4 H3. auto.
  spec IHx2 H6 H7. auto.
  unfold tFun in *.
  congruence.

 Case "XCase".
  inverts_type.
  have (tObj0 = tObj).
   eauto. subst.

  nforall.
  assert (length aa > 0).
  defok ds (DefType tcObj ks dcs).
  nforall.
  lets D: @length_in_in_nonempty H10. rip.
  lists. auto.

  destruct aa. 
   nope.
   have (TYPEA ds ke te se a tObj t1) as HT1.
   have (TYPEA ds ke te se a tObj t2) as HT2.
   spec H a HT1 HT2; burn.

 Case "Alt".
  inverts_type.
  eapply makeTApps_eq_params in H2. rip.
  assert (tsFields0 = tsFields). congruence. subst.
  lets D1: IHx H9 H13; auto.
Qed.


(********************************************************************)
(* Forms of values. 
   If we know the type of a value,
   then we know the form of that value. *)
Lemma value_lam 
 :  forall x ds ke te se t1 t2
 ,  value x 
 -> TYPE ds ke te se x (tFun t1 t2)
 -> (exists t x', x = XLam t x').
Proof.
 intros. destruct x; eauto; nope.

 (* 'x' can't be a XLoc because we don't store naked functions in the heap *)
 Case "XLoc n = XLam t x'".
  inverts H0. 
  unfold tFun in H3.
  simpl in H3. inverts H3.
  nope.
Qed.
Hint Resolve value_lam.


Lemma value_loc
 :  forall x ds ke te se tcObj ts
 ,  value x
 -> isTyConData tcObj
 -> TYPE ds ke te se x (makeTApps (TCon tcObj) ts)
 -> (exists l, x = XLoc l).
Proof.
 intros. destruct x; eauto; nope.
 
 Case "XLAM x = XLoc l".
  inverts_type.
   symmetry in H3.
   apply makeTApps_takeTCon in H3.
   nope.

 Case "XLam t x = XLoc l".
  inverts_type. 
   symmetry in H4.
   unfold tFun in *.
   apply makeTApps_takeTCon in H4.
   simpl in H4.
   inverts H4.
   nope.
Qed.


(********************************************************************)
(* Forms of types *)
Lemma type_XLAM
 :  forall ds ke te se x t
 ,  TYPE ds ke te se (XLAM x) t
 -> (exists t', t = TForall t').
Proof.
 intros. destruct t; nope.
 eauto.
Qed.
Hint Resolve type_XLAM.


Lemma type_XLam
 :  forall ds ke te se x t1 t2
 ,  TYPE ds ke te se (XLam t1 x) t2
 -> (exists t21 t22, t2 = tFun t21 t22).
Proof.
 intros. destruct t2; nope.
 inverts H. 
 unfold tFun. eauto.
Qed.
Hint Resolve type_XLam.


(********************************************************************)
(* A well typed expression is well formed *)

Theorem type_wfX
 :  forall ds ke te se x t
 ,  TYPE ds ke te se x t
 -> wfX (length ke) (length te) (length se) x.
Proof.
 intros. gen ds ke te se t.
 induction x using exp_mutind with 
  (PA := fun a => forall ds ke te se t1 t2
      ,  TYPEA ds ke te se a t1 t2 
      -> wfA (length ke) (length te) (length se) a)
  ; intros; inverts_type; eauto.

 Case "XLAM".
  eapply WfX_XLAM.
  apply IHx in H1.
  rrwrite (length (ke :> KStar) = S (length ke)) in H1.
  rewrite <- length_liftTE in H1.
  rewrite <- length_liftTE in H1.
  auto.

 Case "XLam".
  eapply WfX_XLam; eauto.
  apply IHx in H4.
  rrwrite (length (te :> t) = S (length te)) in H4.
  auto.

 Case "XCon".
  apply WfX_XCon.
  nforall. intros.
  have (exists k, KIND ke x k).
  dest k. eauto.
   nforall. intros.
   have (exists t, TYPE ds ke te se x t).
   dest t. eauto.

 Case "XCase".
  eapply WfX_XCase; eauto.
  nforall. eauto.

 Case "XAlt".
  destruct dc.
  eapply WfA_AAlt. eauto.
  apply IHx in H8.
  rr. lists.
  rrwrite (length te + length tsFields = length tsFields + length te).
  eauto.
Qed.
Hint Resolve type_wfX.


Lemma type_wfX_Forall2
 :  forall ds ke te se xs ts
 ,  Forall2 (TYPE ds ke te se) xs ts
 -> Forall (wfX (length ke) (length te) (length se)) xs.
Proof.
 intros.
 eapply (Forall2_Forall_left (TYPE ds ke te se)).
 intros. nforall. eauto.
 eauto.
Qed.
Hint Resolve type_wfX_Forall2.


(********************************************************************)
(* Weakening Kind Env in Type Judgement. *)
Lemma type_kienv_insert
 :  forall ix ds ke te se x1 t1 k2
 ,  TYPE ds ke                te             se              x1             t1
 -> TYPE ds (insert ix k2 ke) (liftTE ix te) (liftTE ix se) (liftTX ix x1) (liftTT 1 ix t1).
Proof.
 intros. gen ix ds ke te se t1 k2.
 induction x1 using exp_mutind with 
  (PA := fun a => forall ix ds ke te se k2 t3 t4
               ,  TYPEA ds ke te se a t3 t4 
               -> TYPEA ds (insert ix k2 ke) (liftTE ix te)   (liftTE ix se)
                           (liftTA ix a)     (liftTT 1 ix t3) (liftTT 1 ix t4))
  ; intros; inverts_type; simpl; eauto.

 Case "XVar".
  apply TyVar.
  apply get_map; auto.

 Case "XLoc".
  eapply TyLoc; eauto.
  apply get_map; auto.
  burn.

 Case "XLAM".
  eapply TyLAM. 
  rewrite insert_rewind. 
  rewrite (liftTE_liftTE 0 ix).
  rewrite (liftTE_liftTE 0 ix).
  burn.

 Case "XAPP".
  rewrite (liftTT_substTT' 0 ix). 
  simpl.
  eapply TyAPP.
  eapply (IHx1 ix) in H2. simpl in H2. eauto.
  apply kind_kienv_insert; auto.

 Case "XLam".
  apply TyLam.
   apply kind_kienv_insert. auto.
   rrwrite ( liftTE ix te :> liftTT 1 ix t
           = liftTE ix (te :> t)).
   burn.

 Case "XApp".
  eapply TyApp.
   eapply IHx1_1 in H2. simpl in H2. eauto.
   eapply IHx1_2 in H4. eauto.

 Case "XCon".
  (* unpack the data type definition *)
  defok ds (DefData dc tsFields tc).

  (* show XCon has the correct type *)
  rr. eapply TyCon; eauto.

   (* type args have correct kinds *)
    eapply Forall2_map_left.
    eapply Forall2_impl; eauto.
    eauto using kind_kienv_insert.

   (* exp args have correct types *)
    apply Forall2_map.
    apply Forall2_map_right' in H9.
    eapply Forall2_impl_in; eauto.
     simpl. intros.

     nforall.
     lets D: H ix H2 k2; auto. clear H.

     assert ( liftTT 1 ix (substTTs 0 ts y)
            = substTTs 0 (map (liftTT 1 ix) ts) y).

      rrwrite (ix = ix + 0).
      rewrite liftTT_substTTs'.
      rrwrite (ix + 0 = ix).
      f_equal. 

       assert (wfT (length ts) y).
       assert (length ts = length ks) as HLts.
        eapply Forall2_length. eauto.
        rewrite HLts.
        eapply kind_wfT. nforall. eauto.

       rr. apply liftTT_wfT_1. auto.
       rewrite <- H. auto.

 Case "XCase".
  eapply TyCase; eauto.
  apply  Forall_map.
  eapply Forall_impl_in; eauto.
   intros. nforall.
   eapply H; burn.
    rr. burn.
    nforall. intros.
     have (In x (map dcOfAlt aa)).
     rr. auto.

 Case "XUpdate".
  defok ds (DefData dc tsFields tcObj).
  eapply TyUpdate; eauto.
   eapply Forall2_map_left.
    eapply Forall2_impl; eauto.
    eauto using kind_kienv_insert.
  rrwrite (TCon tcObj = liftTT 1 ix (TCon tcObj)).  
  rewrite <- liftTT_makeTApps.
  eauto.    
  rrwrite (ix = 0 + ix).  
   rewrite substTTs_liftTT.
    rr. eauto.
    rr.
    rrwrite (length ts = length ks).
    nforall. eauto.

 Case "XAlt".
  defok ds (DefData dc tsFields tc).

  rr.
  eapply TyAlt; eauto.
   eapply Forall2_map_left.
   eapply Forall2_impl; eauto.
    intros. eapply kind_kienv_insert. auto.

   assert ( map (liftTT 1 ix) (map (substTTs 0 tsParam)  tsFields)
          = map (substTTs 0 (map (liftTT 1 ix) tsParam)) tsFields) as HXX.
    lists.
    erewrite map_ext_in; eauto.
    intros.
     rename x into t1.
     rrwrite (ix = ix + 0).
     rewrite liftTT_substTTs'.
     f_equal. 
     nnat.
     assert (wfT (length ks) t1).
      eapply kind_wfT.
      nforall. eauto.
     eapply liftTT_wfT_1.
     rrwrite (length tsParam = length ks).
     auto.

   unfold liftTE in IHx1. 
   unfold liftTE.
   rewrite <- HXX.
   rewrite <- map_app.
   burn.
Qed.


Lemma type_kienv_weaken1
 :  forall ds ke te se x1 t1 k2
 ,  TYPE ds ke                   te  se            x1            t1
 -> TYPE ds (ke :> k2) (liftTE 0 te) (liftTE 0 se) (liftTX 0 x1) (liftTT 1 0 t1).
Proof.
 intros.
 assert (ke :> k2 = insert 0 k2 ke) as HI.
  simpl. destruct ke; auto.
 rewrite HI.
 eapply type_kienv_insert; auto.
Qed.


(********************************************************************)
(* Weakening Type Env in Type Judgement.
   We can insert a new type into the type environment, provided we
   lift existing references to types higher in the stack across
   the new one. *)
Lemma type_tyenv_insert
 :  forall ds ke te se ix x t1 t2
 ,  TYPE ds ke te se x t1
 -> TYPE ds ke (insert ix t2 te) se (liftXX 1 ix x) t1.
Proof.
 intros. gen ix ds ke se te t1 t2.
 induction x using exp_mutind with 
  (PA := fun a => forall ix ds ke se te t2 t3 t4
      ,  TYPEA ds ke te se a t3 t4 
      -> TYPEA ds ke (insert ix t2 te) se (liftXA 1 ix a) t3 t4)
  ; intros; inverts_type; simpl; eauto.

 Case "XVar".
  nnat; lift_cases; burn.

 Case "XLAM".
  apply TyLAM.
  assert ( liftTE 0 (insert ix t2 te)
         = insert ix (liftTT 1 0 t2) (liftTE 0 te)).
   unfold liftTE. rewrite map_insert. auto.
   burn.

 Case "XLam".
  apply TyLam; eauto.
  rewrite insert_rewind. auto.

 Case "XCon".
  eapply TyCon; eauto.
   nforall.
   apply (Forall2_map_left (TYPE ds ke (insert ix t2 te) se)).
   apply (Forall2_impl_in  (TYPE ds ke te se)); eauto.

 Case "XCase".
  eapply TyCase; eauto.
   apply Forall_map.
   apply (Forall_impl_in 
     (fun a => TYPEA ds ke te se a tObj t1)); eauto.
   nforall. eauto.
   nforall.
    intros. lists.
    rename x0 into d.
    eapply map_exists_in.
    have (In d (map dcOfAlt aa)). 
    assert (exists a, dcOfAlt a = d /\ In a aa).
     eapply map_in_exists. auto.
   shift a. rip.
   eapply dcOfAlt_liftXA.

 Case "XAlt".
  defok ds (DefData dc tsFields tc).
  eapply TyAlt; eauto.
  rewrite insert_app.
  lists. burn.
Qed. 


(* We can push a new type onto the environment stack provided
   we lift references to existing types across the new one. *)
Lemma type_tyenv_weaken1
 :  forall ds ke te se x t1 t2
 ,  TYPE ds ke te se x t1
 -> TYPE ds ke (te :> t2) se (liftXX 1 0 x) t1.
Proof.
 intros.
 assert (te :> t2 = insert 0 t2 te) as HI.
  simpl. destruct te; auto.
 rewrite HI.
 apply type_tyenv_insert. auto.
Qed.


(* We can several new types onto the environment stack provided
   we lift referenes to existing types across the new one. *)
Lemma type_tyenv_weaken_append
 :  forall ds ke te te' se x t1
 ,  TYPE ds ke te se x t1
 -> TYPE ds ke (te >< te') se (liftXX (length te') 0 x) t1.
Proof.
 intros.
 induction te'; simpl.
  burn.

  rrwrite (S (length te') = length te' + 1).
  rrwrite (length te' + 1 = 1 + length te').
  rewrite <- liftXX_plus.
  eapply type_tyenv_weaken1.
  burn.
Qed.


(********************************************************************)
(* Weakening Store Typing in Type Judgement. *)
Lemma type_stenv_snoc 
 :  forall ds ke te se t2    x t1
 ,  closedT t2
 -> TYPE ds ke te se         x t1
 -> TYPE ds ke te (t2 <: se) x t1.
Proof.
 intros. gen ds ke te se t1 t2.
 induction x using exp_mutind with 
  (PA := fun a => forall ds ke te se t2 t3 t4
      ,  closedT t2
      -> TYPEA ds ke te se         a t3 t4
      -> TYPEA ds ke te (t2 <: se) a t3 t4)
  ; intros; inverts_type; eauto.

 Case "XLAM".
  eapply TyLAM.
  unfold liftTE in *. rr.
  spec IHx H2. eauto.

 Case "XCon".
  eapply TyCon; eauto.
  eapply Forall2_impl_in with (R1 := TYPE ds ke te se); 
   nforall; eauto.

 Case "XCase".
  eapply TyCase; 
   nforall; eauto.
Qed.
Hint Resolve type_stenv_snoc.


Lemma type_stenv_weaken
 :  forall ds ke te se1 se2 x t1
 ,  Forall closedT se2
 -> TYPE ds ke te  se1         x t1
 -> TYPE ds ke te (se2 >< se1) x t1.
Proof.
 intros. gen ds ke te se1.
 induction se2; intros.
 rewrite app_nil_right. auto.
 rrwrite ((se2 :> a) >< se1 = se2 >< (a <: se1)). 
 inverts H. eauto.
Qed.
Hint Resolve type_stenv_weaken.


Lemma type_stenv_extends
 :  forall ds ke te se1 se2 x t1
 ,  Forall closedT se2
 -> extends se2 se1
 -> TYPE ds ke te se1 x t1
 -> TYPE ds ke te se2 x t1.
Proof.
 intros.
 unfold extends in *.
 destruct H0 as [se3]. subst.
 eapply type_stenv_weaken; auto.
  eauto.
Qed.
Hint Resolve type_stenv_extends.


Lemma typea_stenv_extends
 :  forall ds ke te aa t1 t2 se1 se2
 ,  Forall closedT se2
 -> extends se2 se1
 -> TYPEA ds ke te se1 aa t1 t2
 -> TYPEA ds ke te se2 aa t1 t2.
Proof.
 intros.
 destruct aa.
 inverts H1.
 eauto.
Qed.
Hint Resolve typea_stenv_extends.

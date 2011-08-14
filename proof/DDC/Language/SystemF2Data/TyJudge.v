
Require Export DDC.Language.SystemF2.Ki.
Require Export DDC.Language.SystemF2.KiJudge.
Require Export DDC.Language.SystemF2.TyEnv.
Require Export DDC.Language.SystemF2.TySubsts.
Require Export DDC.Language.SystemF2Data.Def.
Require Export DDC.Language.SystemF2Data.Exp.
Require Import Coq.Logic.FunctionalExtensionality.


(* Type Judgement assigns a type to an expression. *)
Inductive TYPE (ds: defs) (ke: kienv) (te: tyenv) : exp -> ty -> Prop :=
 (* Variables *)
 | TYVar 
   :  forall i t
   ,  get i te = Some t
   -> TYPE ds ke te (XVar i) t

 (* Type abstraction *)
 | TYLAM 
   :  forall x1 t1
   ,  TYPE ds (ke :> KStar) (liftTE 0 te) x1 t1 
   -> TYPE ds ke             te           (XLAM x1) (TForall t1)

 (* Type application *)
 | TYAPP
   :  forall x1 t1 t2
   ,  TYPE ds ke te x1 (TForall t1)
   -> KIND ke t2 KStar
   -> TYPE ds ke te (XAPP x1 t2) (substTT 0 t2 t1)

 (* Function abstraction *)
 | TYLam
   :  forall x t1 t2
   ,  KIND ke t1 KStar
   -> TYPE ds ke (te :> t1) x            t2
   -> TYPE ds ke te         (XLam t1 x) (tFun t1 t2)

 (* Applications *)
 | TYApp
   :  forall x1 x2 t1 t2
   ,  TYPE ds ke te x1           (tFun t1 t2)
   -> TYPE ds ke te x2           t1
   -> TYPE ds ke te (XApp x1 x2) t2

 (* Data Constructors *)
 | TYCon 
   :  forall tc (ks: list ki) tsFields tsParam xs dc dcs
   ,  DEFSOK ds
   -> getTypeDef tc ds = Some (DefDataType tc ks       dcs)
   -> getDataDef dc ds = Some (DefData     dc tsFields tc)
   -> Forall2 (KIND ke) tsParam ks
   -> Forall2 (TYPE ds ke te) xs (map (substTTs 0 tsParam) tsFields)
   -> TYPE ds ke te (XCon dc tsParam xs) (makeTApps (TCon tc) tsParam)

 (* Case Expressions *)
 | TYCase
   :  forall xObj tObj tcObj ks tResult alts dcs

      (* check types of expression and alternatives *)
   ,  TYPE ds ke te xObj tObj
   -> Forall (fun alt => TYPEA ds ke te alt tObj tResult) alts

      (* All data cons must have a corresponding alternative. 
         Maybe we should move this to the well-formedness judgement *)
   -> getCtorOfType tObj  = Some tcObj
   -> getTypeDef tcObj ds = Some (DefDataType tcObj ks dcs)
   -> Forall (fun dc => In dc (map dcOfAlt alts)) dcs

   -> TYPE ds ke te (XCase xObj alts) tResult

with TYPEA  (ds: defs) (ke: kienv) (te: tyenv) : alt -> ty -> ty -> Prop :=
 (* Case Alternatives *)
 | TYAlt 
   :  forall x1 dc tObj tObj1 tc ks tsParam tsFields tResult dcs
   ,  DEFSOK ds
   -> getTypeDef tc ds = Some (DefDataType tc ks dcs)
   -> getDataDef dc ds = Some (DefData     dc tsFields tc)
   -> Forall2 (KIND ke) tsParam ks
   -> takeTApps tObj   = (tObj1, tsParam)
   -> TYPE  ds ke (te >< map (substTTs 0 tsParam) tsFields) x1 tResult
   -> TYPEA ds ke te (AAlt dc x1) tObj tResult.

Hint Constructors TYPE.
Hint Constructors TYPEA.


(* Invert all hypothesis that are compound typing statements. *)
Ltac inverts_type :=
 repeat 
  (match goal with 
   | [ H: TYPE  _ _ _ (XVar  _)     _    |- _ ] => inverts H
   | [ H: TYPE  _ _ _ (XLAM  _)     _    |- _ ] => inverts H
   | [ H: TYPE  _ _ _ (XAPP  _ _)   _    |- _ ] => inverts H
   | [ H: TYPE  _ _ _ (XLam  _ _)   _    |- _ ] => inverts H
   | [ H: TYPE  _ _ _ (XApp  _ _)   _    |- _ ] => inverts H
   | [ H: TYPE  _ _ _ (XCon  _ _ _) _    |- _ ] => inverts H
   | [ H: TYPE  _ _ _ (XCase _ _)   _    |- _ ] => inverts H
   | [ H: TYPEA _ _ _ (AAlt _ _)    _ _  |- _ ] => inverts H
   end).


(********************************************************************)
(* Forms of values. 
   If we know the type of a value,
   then we know the form of that value. *)
Lemma value_lam 
 :  forall x ds ke te t1 t2
 ,  value x 
 -> TYPE ds ke te x (tFun t1 t2)
 -> (exists t x', x = XLam t x').
Proof.
 intros. destruct x; eauto; nope.

 (* Show that x can't be a XCon because data type definitions can't
    define the function type constructor. *)
 unfold tFun in H0.
 inverts H0.
  apply makeTApps_takeTCon in H4.
  simpl in H4. inverts H4.
  assert (DEFOK ds (DefData d tsFields TyConFun)).
   eapply getDataDef_ok; eauto.
  inverts H0. nope.
Qed.
Hint Resolve value_lam.


(********************************************************************)
(* A well typed expression is well formed *)

Theorem type_wfX
 :  forall ds ke te x t
 ,  TYPE ds ke te x t
 -> wfX (length ke) (length te) x.
Proof.
 intros. gen ds ke te t.
 induction x using exp_mutind with 
  (PA := fun a => forall ds ke te t1 t2
      ,  TYPEA ds ke te a t1 t2 
      -> wfA (length ke) (length te) a)
  ; intros; inverts_type; eauto.

 Case "XVar".
  eapply WfX_XVar. 
   eapply get_length_less. eauto.

 Case "XLAM".
  eapply WfX_XLAM.
  apply IHx in H1.
  assert (length (ke :> KStar) = S (length ke)).
   auto. rewrite H in H1.
   rewrite <- length_liftTE in H1. auto.

 Case "XLam".
  eapply WfX_XLam.
  eauto.
  apply IHx in H4. 
   assert (length (te :> t) = S (length te)).
    auto. rewrite H in H4. auto.
  
 Case "XCon".
  apply WfX_XCon.
  nforall. intros.
  eapply Forall2_exists_left in H8; eauto.
  dest y. eauto.
   nforall. intros.
   eapply Forall2_exists_left in H9; eauto. dest y.
   eapply H; eauto.

 Case "XCase".
  eapply WfX_XCase.
   eapply IHx. eauto.
   nforall. eauto.

 Case "XAlt".
  destruct dc.
  eapply WfA_AAlt. eauto.
  apply IHx in H9.
  rewrite app_length in H9.
  rewrite map_length in H9.
  rewrite plus_comm  in H9.
  auto.   
Qed.
Hint Resolve type_wfX.


(********************************************************************)
(* Weakening Kind Env in Type Judgement. *)
Lemma type_kienv_insert
 :  forall ix ds ke te x1 t1 k2
 ,  TYPE ds ke                te             x1             t1
 -> TYPE ds (insert ix k2 ke) (liftTE ix te) (liftTX ix x1) (liftTT 1 ix t1).
Proof.
 intros. gen ix ds ke te t1 k2.
 induction x1 using exp_mutind with 
  (PA := fun a => forall ix ds ke te k2 t3 t4
               ,  TYPEA ds ke te a t3 t4 
               -> TYPEA ds (insert ix k2 ke) (liftTE ix te) 
                           (liftTA ix a)     (liftTT 1 ix t3) (liftTT 1 ix t4))
  ; intros; inverts_type; simpl; eauto.

 Case "XVar".
  apply TYVar.
  apply get_map; auto.

 Case "XLAM".
  eapply TYLAM. 
  rewrite insert_rewind. 
   rewrite (liftTE_liftTE 0 ix).
   apply IHx1. auto.

 Case "XAPP".
  rewrite (liftTT_substTT' 0 ix). simpl.
  eapply TYAPP.
  eapply (IHx1 ix) in H2. simpl in H2. eauto.
  apply kind_kienv_insert. auto.

 Case "XLam".
  apply TYLam.
   apply kind_kienv_insert. auto.
   assert ( liftTE ix te :> liftTT 1 ix t
          = liftTE ix (te :> t)). auto. rewrite H. clear H.
   apply IHx1. auto.

 Case "XApp".
  eapply TYApp.
   eapply IHx1_1 in H2. simpl in H2. eauto.
   eapply IHx1_2 in H4. eauto.

 Case "XCon".
  (* unpack the data type definition *)
  assert (DEFOK ds (DefData dc tsFields tc)) as DDefOK.
   eapply getDataDef_ok; eauto.
   inverts DDefOK.
   assert (ks0 = ks /\ dcs0 = dcs).
    rewrite H5 in H3. inverts H3. auto. int. subst. clear H3.

  (* show XCon has the correct type *)
  rewrite liftTT_makeTApps.
  eapply TYCon; eauto.

   (* type args have correct kinds *)
    eapply Forall2_map_left.
    eapply Forall2_impl; eauto.
    intros. eapply kind_kienv_insert. auto.

   (* exp args have correct types *)
    apply Forall2_map.
    apply Forall2_map_right' in H9.
    eapply Forall2_impl_in; eauto.
     simpl. intros.

    nforall.
    lets D: H ix H2 k2; auto. clear H.

    assert ( liftTT 1 ix (substTTs 0 ts y)
           = substTTs 0 (map (liftTT 1 ix) ts) y).

     assert (ix = ix + 0) as Hix0. omega.  
     rewrite -> Hix0. 
     rewrite liftTT_substTTs'. 
     rewrite <- Hix0.
     f_equal.

    assert (wfT (length ts) y).
     assert (length ts = length ks) as HLts.
      eapply Forall2_length. eauto.
     rewrite HLts.
     eapply kind_wfT. nforall. eauto.
     nnat.
     apply liftTT_wfT_1. auto.

    rewrite <- H. auto.

 Case "XCase".
  eapply TYCase; eauto.
  apply  Forall_map.
  eapply Forall_impl_in; eauto.
   intros. nforall.
   eapply H. 
    auto.
    simpl in H1. auto.
    rewrite liftTT_getCtorOfType. auto.
    nforall. intros.
     apply H8 in H0.
     rewrite dcOfAlt_liftTA_map. auto.

 Case "XAlt".
  assert ( takeTApps (liftTT 1 ix t3) 
          = (liftTT 1 ix tObj1, map (liftTT 1 ix) tsParam)).
   unfold takeTApps in H6. invert H6. intros.
   unfold takeTApps. f_equal. auto. auto.

  lets HD: getDataDef_ok H2 H4. inverts HD.
   rewrite H8 in H3. inverts H3.

  eapply TYAlt; eauto. 
   unfold takeTApps in H. invert H. intros.
   unfold takeTApps.
   rewrite H3. lists. auto.
   eapply Forall2_map_left.
   eapply Forall2_impl; eauto.
    intros. eapply kind_kienv_insert. auto.

   assert ( map (liftTT 1 ix) (map (substTTs 0 tsParam)  tsFields)
          = map (substTTs 0 (map (liftTT 1 ix) tsParam)) tsFields) as HXX.
    rewrite map_map. unfold compose.
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
     assert (length tsParam = length ks) as HL; eauto.
     rewrite HL. auto.

   rewrite <- HXX.
   unfold liftTE. rewrite <- map_app.
   unfold liftTE in IHx1. eapply IHx1. auto.
Qed.   


(********************************************************************)
(* Weakening Type Env in Type Judgement.
   We can insert a new type into the type environment, provided we
   lift existing references to types higher in the stack across
   the new one. *)
Lemma type_tyenv_insert
 :  forall ds ke te ix x t1 t2
 ,  TYPE ds ke te x t1
 -> TYPE ds ke (insert ix t2 te) (liftXX 1 ix x) t1.
Proof.
 intros. gen ix ds ke te t1 t2.
 induction x using exp_mutind with 
  (PA := fun a => forall ix ds ke te t2 t3 t4
      ,  TYPEA ds ke te a t3 t4 
      -> TYPEA ds ke (insert ix t2 te) (liftXA 1 ix a) t3 t4)
  ; intros; inverts_type; simpl; eauto.

 Case "XVar".
  nnat. 
  lift_cases; intros; auto.

 Case "XLAM".
  apply TYLAM.
  assert ( liftTE 0 (insert ix t2 te)
         = insert ix (liftTT 1 0 t2) (liftTE 0 te)).
   unfold liftTE. rewrite map_insert. auto.
   rewrite H. eauto.

 Case "XLam".
  apply TYLam; eauto.
  rewrite insert_rewind. auto.

 Case "XCon".
  eapply TYCon; eauto.
   nforall.
   apply (Forall2_map_left (TYPE ds ke (insert ix t2 te))).
   apply (Forall2_impl_in  (TYPE ds ke te)); eauto.

 Case "XCase".
  eapply TYCase; eauto.
   apply Forall_map.
   apply (Forall_impl_in 
     (fun a => TYPEA ds ke te a tObj t1)); eauto.
   nforall. eauto.
   nforall.
    intros. lists.
    rename x0 into d.
    eapply map_exists_in.
    assert (In d (map dcOfAlt aa)). 
     eauto.
    assert (exists a, dcOfAlt a = d /\ In a aa).
     eapply map_in_exists. auto.
   shift a. subst. int. rewrite <- H7.
   eapply dcOfAlt_liftXA.

 Case "XAlt".
  destruct dc.
  eapply TYAlt; eauto.
  rewrite insert_app.
  lists.
  eapply getDataDef_ok in H2; eauto.
   inverts H2. auto.
Qed. 


(* We can push a new type onto the environment stack provided
   we lift references to existing types across the new one. *)
Lemma type_tyenv_weaken1
 :  forall ds ke te x t1 t2
 ,  TYPE ds ke te x t1
 -> TYPE ds ke (te :> t2) (liftXX 1 0 x) t1.
Proof.
 intros.
 assert (te :> t2 = insert 0 t2 te).
  simpl. destruct te; auto.
 rewrite H0.
  apply type_tyenv_insert. auto.
Qed.


(* We can several new types onto the environment stack provided
   we lift referenes to existing types across the new one. *)
Lemma type_tyenv_weaken_append
 :  forall ds ke te te' x t1
 ,  TYPE ds ke te x t1
 -> TYPE ds ke (te >< te') (liftXX (length te') 0 x) t1.
Proof.
 intros.
 induction te'; simpl.
  rewrite liftXX_zero. 
   auto. 
  rewrite <- nat_plus_one.
   assert (length te' + 1 = 1 + length te') as HL. 
    burn. rewrite HL. clear HL.
   rewrite <- liftXX_plus.
   eapply type_tyenv_weaken1. auto. 
Qed.

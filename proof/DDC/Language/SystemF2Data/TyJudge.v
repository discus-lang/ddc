
Require Export DDC.Language.SystemF2.Ki.
Require Export DDC.Language.SystemF2.KiJudge.
Require Export DDC.Language.SystemF2.TyEnv.
Require Export DDC.Language.SystemF2Data.Def.
Require Export DDC.Language.SystemF2Data.Exp.


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
   :  forall x1 dc tObj tObj1 tsObj tsFields tResult tcObj
   ,  DEFSOK ds
   -> getDataDef dc ds = Some (DefData dc tsFields tcObj)
   -> takeTApps tObj   = (tObj1, tsObj)
   -> TYPE  ds ke (te >< map (substTTs 0 tsObj) tsFields) x1 tResult
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
   | [ H: TYPEA _ _ _ (AAlt _ _ _)  _ _  |- _ ] => inverts H
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
  inverts H.
  eapply WfA_AAlt. eauto.
  apply IHx in H7.
  rewrite app_length in H7.
  rewrite map_length in H7.
  rewrite plus_comm  in H7.
  auto.   
Qed.
Hint Resolve type_wfX.



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
    intros. rewrite map_map. unfold compose.
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
  inverts H.
  eapply TYAlt; eauto.
  rewrite insert_app.
  rewrite map_length.
  apply getDataDef_ok in H3; auto.
   inverts H3. auto.
Qed. 


(*
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

*)
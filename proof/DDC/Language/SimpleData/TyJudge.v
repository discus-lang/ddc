
Require Export DDC.Language.SimpleData.Exp.


(* Type Judgement assigns a type to an expression. *)
Inductive TYPE : defs -> tyenv -> exp -> ty -> Prop :=
 (* Variables *)
 | TYVar 
   :  forall ds te i t
   ,  get i te = Some t
   -> TYPE ds te (XVar i) t

 (* Lambda Abstraction *)
 | TYLam
   :  forall ds te x t1 t2
   ,  TYPE ds (te :> t1) x            t2
   -> TYPE ds te         (XLam t1 x) (TFun t1 t2)

 (* Applications *)
 | TYApp
   :  forall ds te x1 x2 t1 t2
   ,  TYPE ds te x1           (TFun t1 t2)
   -> TYPE ds te x2           t1
   -> TYPE ds te (XApp x1 x2) t2

 (* Data Constructors *)
 | TYCon 
   :  forall ds te xs dc dcs tsArgs tc
   ,  getDataDef dc ds = Some (DefData     dc tsArgs (TCon tc))
   -> getTypeDef tc ds = Some (DefDataType tc dcs)
   -> In dc dcs
   -> Forall2 (TYPE ds te) xs tsArgs
   -> TYPE ds te (XCon dc xs) (TCon tc)

 (* Case Expressions *)
 | TYCase
   :  forall ds te xObj tcPat tResult alts dcs

      (* check types of expression and alternatives *)
   ,  TYPE ds te xObj (TCon tcPat)            
   -> Forall (fun alt => TYPEA ds te alt (TCon tcPat) tResult) alts

      (* there must be at least one alternative *)
   -> length alts > 0

      (* all data cons must have a corresponding alternative *)
   -> getTypeDef tcPat ds = Some (DefDataType tcPat dcs)
   -> Forall (fun dc => In dc (map dcOfAlt alts)) dcs

   -> TYPE ds te (XCase xObj alts) tResult


with TYPEA : defs -> tyenv -> alt -> ty -> ty -> Prop :=
 (* Case Alternatives *)
 | TYAlt 
   :  forall ds te x1 t1 dc tsArgs tResult
   ,  getDataDef dc ds = Some (DefData dc tsArgs tResult)
   -> TYPE  ds (te >< tsArgs) x1 t1
   -> TYPEA ds te (AAlt dc tsArgs x1) tResult t1.

Hint Constructors TYPE.
Hint Constructors TYPEA.


(* Invert all hypothesis that are compound typing statements. *)
Ltac inverts_type :=
 repeat 
  (match goal with 
   | [ H: TYPE _ _ (XVar  _)   _      |- _ ] => inverts H
   | [ H: TYPE _ _ (XLam  _ _) _      |- _ ] => inverts H
   | [ H: TYPE _ _ (XApp  _ _) _      |- _ ] => inverts H
   | [ H: TYPE _ _ (XCon  _ _) _      |- _ ] => inverts H
   | [ H: TYPE _ _ (XCase _ _) _      |- _ ] => inverts H
   | [ H: TYPEA _ _ (AAlt _ _ _) _ _  |- _ ] => inverts H
   end).


(* A well typed expression is well formed *)
Theorem type_wfX
 :  forall ds te x t
 ,  TYPE ds te x t
 -> wfX te x.
Proof.
 intros. gen ds te t.
 induction x using exp_mutind with 
  (PA := fun a => forall ds te t1 t2
      ,  TYPEA ds te a t1 t2 
      -> wfA te a)
  ; intros; inverts_type; eauto.

 Case "XCon".
  apply WfX_XCon.
  nforall. intros.
  eapply Forall2_exists_left in H9; eauto.
  destruct H9. eauto.

 Case "XCase".
  eapply WfX_XCase.
   eapply IHx. eauto.
   nforall. eauto.
Qed.
Hint Resolve type_wfX.


(* Weakening Type Env in Type Judgement.
   We can insert a new type into the type environment, provided we
   lift existing references to types higher in the stack across
   the new one. *)
Lemma type_tyenv_insert
 :  forall ds te ix x t1 t2
 ,  TYPE ds te x t1
 -> TYPE ds (insert ix t2 te) (liftX 1 ix x) t1.
Proof.
 intros. gen ix ds te t1.
 induction x using exp_mutind with 
  (PA := fun a => forall ix ds te t3 t4
      ,  TYPEA ds te a t3 t4 
      -> TYPEA ds (insert ix t2 te) (liftA 1 ix a) t3 t4)
  ; intros; inverts_type; simpl; eauto.

 Case "XVar".
  nnat. 
  lift_cases; intros; auto.

 Case "XLam".
  apply TYLam.
  rewrite insert_rewind. auto.

 Case "XCon".
  eapply TYCon; eauto.
   nforall.
   apply (Forall2_map_left (TYPE ds (insert ix t2 te))).
   apply (Forall2_impl_in  (TYPE ds te)); eauto.

 Case "XCase".
  eapply TYCase; eauto.
   apply Forall_map.
   apply (Forall_impl_in (fun a => TYPEA ds te a (TCon tcPat) t1)); eauto.
   nforall. eauto.

  rewrite map_length; auto.

  nforall.
   intros. rename x0 into d. 
   rewrite map_map. unfold compose.
   eapply map_exists_in.
   assert (In d (map dcOfAlt aa)). 
    eauto.
   assert (exists a, dcOfAlt a = d /\ In a aa).
    eapply map_in_exists. auto.
   shift a. subst. int. rewrite <- H6.
   eapply dcOfAlt_liftA.

 Case "XAlt".
  eapply TYAlt; eauto.
  rewrite insert_app. auto.
Qed. 


(* We can push a new type onto the environment stack provided
   we lift references to existing types across the new one. *)
Lemma type_tyenv_weaken1
 :  forall ds te x t1 t2
 ,  TYPE ds te x t1
 -> TYPE ds (te :> t2) (liftX 1 0 x) t1.
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
 :  forall ds te te' x t1
 ,  TYPE ds te x t1
 -> TYPE ds (te >< te') (liftX (length te') 0 x) t1.
Proof.
 intros.
 induction te'; simpl.
  rewrite liftX_zero. 
   auto. 
  rewrite <- nat_plus_one.
   assert (length te' + 1 = 1 + length te') as HL. 
    burn. rewrite HL. clear HL.
   rewrite <- liftX_plus.
   eapply type_tyenv_weaken1. auto. 
Qed.


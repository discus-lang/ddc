
Require Export DDC.Language.SystemF2.Ki.
Require Export DDC.Language.SystemF2.TyEnv.
Require Export DDC.Language.SystemF2Data.ExpBase.
Require Export DDC.Language.SystemF2Data.ExpLift.
Require Export DDC.Language.SystemF2Data.ExpSubst.
Require Export DDC.Language.SystemF2Data.ExpAlt.


(* Weak normal forms cannot be reduced further by 
   call-by-value evaluation. *)
Inductive wnfX : exp -> Prop :=
 | Wnf_XVar 
   : forall i
   , wnfX (XVar i)

 | Wnf_XLAM
   : forall x1
   , wnfX (XLAM x1)

 | Wnf_XLam
   : forall t1 x2
   , wnfX (XLam t1 x2)

 | Wnf_XCon
   :  forall dc xs
   ,  Forall wnfX xs
   -> wnfX (XCon dc xs).
Hint Constructors wnfX.


(* Well formed expressions are closed under the given environment. *)
Inductive wfX : kienv -> tyenv -> exp -> Prop :=
 | WfX_XVar 
   :  forall ke te i
   ,  (exists t, get i te = Some t)
   -> wfX ke te (XVar i)
 
 | WfX_XLAM
   :  forall ke te x
   ,  wfX (ke :> KStar) (liftTE 0 te) x
   -> wfX ke te (XLAM x)

 | WfX_XAPP
   :  forall ke te x1 t2
   ,  wfX ke te x1 -> wfT ke t2
   -> wfX ke te (XAPP x1 t2)

 | WfX_XLam
   :  forall ke te t1 x2
   ,  wfT ke t1 -> wfX ke (te :> t1) x2
   -> wfX ke te (XLam t1 x2)

 | WfX_XApp 
   :  forall ke te x1 x2
   ,  wfX ke te x1 -> wfX ke te x2
   -> wfX ke te (XApp x1 x2)

 | WfX_XCon
   :  forall ke te dc xs
   ,  Forall (wfX ke te) xs
   -> wfX ke te (XCon dc xs)

 | WfX_XCase
   :  forall ke te x alts
   ,  wfX ke te x 
   -> Forall (wfA ke te) alts
   -> wfX ke te (XCase x alts)

with    wfA : kienv -> tyenv -> alt -> Prop :=
 | WfA_AAlt
   :  forall ke te dc ds ts x tsArgs tResult
   ,  getDataDef dc ds = Some (DefData dc tsArgs tResult)
   -> wfX ke (te >< tsArgs) x
   -> wfA ke te (AAlt dc ts x).

Hint Constructors wfX.
Hint Constructors wfA.


(* Closed expressions are well formed under an empty environment. *)
Definition closedX (xx: exp) : Prop
 := wfX nil nil xx.
Hint Unfold closedX.


(* Values are closed expressions that cannot be reduced further. *)
Inductive value : exp -> Prop :=
 | Value 
   :  forall xx
   ,  wnfX xx -> closedX xx
   -> value xx.
Hint Constructors value.

Lemma value_wnfX 
 : forall xx, value xx -> wnfX xx.
 Proof. intros. inverts H. auto. Qed.
Hint Resolve value_wnfX.

Lemma value_closedX 
 : forall xx, value xx -> closedX xx.
 Proof. intros. inverts H. auto. Qed.
Hint Resolve value_closedX.

Lemma value_wnfXs_XCon
 : forall xs dc
 , value (XCon dc xs) -> Forall wnfX xs.
Proof.
 intros. inverts H. inverts H0. auto.
Qed.
Hint Resolve value_wnfXs_XCon.

Lemma value_closedXs_XCon
 : forall xs dc
 , value (XCon dc xs) -> Forall closedX xs.
Proof.
 intros. inverts H. inverts H1. auto.
Qed.
Hint Resolve value_closedXs_XCon.


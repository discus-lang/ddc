
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
   :  forall dc ts xs
   ,  Forall wnfX xs
   -> wnfX (XCon dc ts xs).
Hint Constructors wnfX.


(* Well formed expressions are closed under the given environment. *)
Inductive wfX (kn: nat) (tn: nat) : exp -> Prop :=
 | WfX_XVar 
   :  forall ti
   ,  ti < tn
   -> wfX kn tn (XVar ti)
 
 | WfX_XLAM
   :  forall x
   ,  wfX (S kn) tn x
   -> wfX kn tn (XLAM x)

 | WfX_XAPP
   :  forall x1 t2
   ,  wfX kn tn x1 -> wfT kn t2
   -> wfX kn tn (XAPP x1 t2)

 | WfX_XLam
   :  forall t1 x2
   ,  wfT kn t1 -> wfX kn (S tn) x2
   -> wfX kn tn (XLam t1 x2)

 | WfX_XApp 
   :  forall x1 x2
   ,  wfX kn tn x1 -> wfX kn tn x2
   -> wfX kn tn (XApp x1 x2)

 | WfX_XCon
   :  forall dc ts xs
   ,  Forall (wfT kn)    ts
   -> Forall (wfX kn tn) xs
   -> wfX kn tn (XCon dc ts xs)

 | WfX_XCase
   :  forall x alts
   ,  wfX kn tn x 
   -> Forall (wfA kn tn) alts
   -> wfX kn tn (XCase x alts)

with    wfA (kn: nat) (tn: nat) : alt -> Prop :=
 | WfA_AAlt
   :  forall dc ds x tsArgs tResult
   ,  getDataDef dc ds = Some (DefData dc tsArgs tResult)
   -> wfX kn (tn + length tsArgs) x
   -> wfA kn tn (AAlt dc x).

Hint Constructors wfX.
Hint Constructors wfA.


(* Closed expressions are well formed under an empty environment. *)
Definition closedX (xx: exp) : Prop
 := wfX O O xx.
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
 : forall ts xs dc
 , value (XCon dc ts xs) -> Forall wnfX xs.
Proof.
 intros. inverts H. inverts H0. auto.
Qed.
Hint Resolve value_wnfXs_XCon.

Lemma value_closedXs_XCon
 : forall ts xs dc
 , value (XCon dc ts xs) -> Forall closedX xs.
Proof.
 intros. inverts H. inverts H1. auto.
Qed.
Hint Resolve value_closedXs_XCon.


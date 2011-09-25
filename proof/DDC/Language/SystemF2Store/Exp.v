
Require Export DDC.Language.SystemF2.Ki.
Require Export DDC.Language.SystemF2.TyEnv.
Require Export DDC.Language.SystemF2Store.ExpBase.
Require Export DDC.Language.SystemF2Store.ExpLift.
Require Export DDC.Language.SystemF2Store.ExpSubst.
Require Export DDC.Language.SystemF2Store.ExpAlt.


(* Weak normal forms cannot be reduced further by 
   call-by-value evaluation. *)
Inductive wnfX : exp -> Prop :=
 | Wnf_XVar 
   : forall i
   , wnfX (XVar i)

 | Wnf_XLoc
   : forall i
   , wnfX (XLoc i)

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
Inductive wfX (kn: nat) (tn: nat) (sn: nat) : exp -> Prop :=
 | WfX_XVar 
   :  forall ti
   ,  ti < tn
   -> wfX kn tn sn (XVar ti)
 
 | WfX_XLoc
   :  forall li
   ,  li < sn
   -> wfX kn tn sn (XLoc li) 

 | WfX_XLAM
   :  forall x
   ,  wfX (S kn) tn sn x
   -> wfX kn tn sn (XLAM x)

 | WfX_XAPP
   :  forall x1 t2
   ,  wfX kn tn sn x1 -> wfT kn t2
   -> wfX kn tn sn (XAPP x1 t2)

 | WfX_XLam
   :  forall t1 x2
   ,  wfT kn t1 -> wfX kn (S tn) sn x2
   -> wfX kn tn sn (XLam t1 x2)

 | WfX_XApp 
   :  forall x1 x2
   ,  wfX kn tn sn x1 -> wfX kn tn sn x2
   -> wfX kn tn sn (XApp x1 x2)

 | WfX_XCon
   :  forall dc ts xs
   ,  Forall (wfT kn)       ts
   -> Forall (wfX kn tn sn) xs
   -> wfX kn tn sn (XCon dc ts xs)

 | WfX_XCase
   :  forall x alts
   ,  wfX kn tn sn x 
   -> Forall (wfA kn tn sn) alts
   -> wfX kn tn sn (XCase x alts)

  | WfX_XUpdate
    :  forall c i ts x1 x2
    ,  wfX kn tn sn x1 
    -> wfX kn tn sn x2
    -> wfX kn tn sn (XUpdate c i ts x1 x2)

with    wfA (kn: nat) (tn: nat) (sn: nat) : alt -> Prop :=
 | WfA_AAlt
   :  forall dc ds x tsArgs tResult
   ,  getDataDef dc ds = Some (DefData dc tsArgs tResult)
   -> wfX kn (tn + length tsArgs) sn x
   -> wfA kn tn sn (AAlt dc x).

Hint Constructors wfX.
Hint Constructors wfA.


(* Closed expressions are well formed under
   empty type and kind environments, but may contain store locations. *)
Definition closedX (xx: exp) : Prop
 := exists sn, wfX O O sn xx.
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
 intros. 
 inverts H.
 unfold closedX in H1. dest sn.
 inverts H.
 nforall. eauto. 
Qed.
Hint Resolve value_closedXs_XCon.


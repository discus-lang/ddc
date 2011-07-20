
Require Export DDC.Language.SimpleData.ExpBase.
Require Export DDC.Language.SimpleData.ExpLift.
Require Export DDC.Language.SimpleData.ExpSubst.
Require Export DDC.Language.SimpleData.ExpAlt.

(* Weak normal forms cannot be reduced further by 
   call-by-value evaluation. *)
Inductive wnfX : exp -> Prop :=
 | Wnf_XVar 
   : forall i
   , wnfX (XVar i)

 | Wnf_XLam
   : forall t1 x2
   , wnfX (XLam t1 x2)

 | Wnf_XCon
   :  forall dc xs
   ,  Forall wnfX xs
   -> wnfX (XCon dc xs).
Hint Constructors wnfX.


(* Well formed expressions are closed under the given environment. *)
Inductive wfX : tyenv -> exp -> Prop :=
 | WfX_XVar 
   :  forall te i
   ,  (exists t, get i te = Some t)
   -> wfX te (XVar i)
 
 | WfX_XLam
   :  forall te t x
   ,  wfX (te :> t) x
   -> wfX te (XLam t x)

 | WfX_XApp 
   :  forall te x1 x2
   ,  wfX te x1 -> wfX te x2
   -> wfX te (XApp x1 x2)

 | WfX_XCon
   :  forall te dc xs
   ,  Forall (wfX te) xs
   -> wfX te (XCon dc xs)

 | WfX_XCase
   :  forall te x alts
   ,  wfX te x 
   -> Forall (wfA te) alts
   -> wfX te (XCase x alts)

with    wfA : tyenv -> alt -> Prop :=
 | WfA_AAlt
   :  forall te dc ds ts x tsArgs tResult
   ,  getDataDef dc ds = Some (DefData dc tsArgs tResult)
   -> wfX (te >< tsArgs) x
   -> wfA te (AAlt dc ts x).

Hint Constructors wfX.
Hint Constructors wfA.


(* Closed expressions are well formed under an empty environment. *)
Definition closedX (xx: exp) : Prop
 := wfX nil xx.
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


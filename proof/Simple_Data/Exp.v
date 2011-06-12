
Require Export ExpBase.
Require Export ExpLift.
Require Export ExpSubst.

(* Weak Head Normal Form ********************************************)
(* Weak Head Normal Forms cannot be reduced further by 
   call-by-value evaluation.
 *)
Inductive whnfX : exp -> Prop :=
 | Whnf_XVar 
   : forall i
   , whnfX (XVar i)

 | Whnf_XLam
   : forall t1 x2
   , whnfX (XLam t1 x2)

 | Whnf_XCon
   :  forall dc xs
   ,  Forall whnfX xs
   -> whnfX (XCon dc xs).
Hint Constructors whnfX.


(* Well Formedness **************************************************)
Inductive wfX : tyenv -> exp -> Prop :=
 | WfX_XVar 
   :  forall te i
   ,  (exists t, BaseEnv.get te i = Some t)
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
   -> wfX (te ++ envOfList tsArgs) x
   -> wfA te (AAlt dc ts x).

Hint Constructors wfX.
Hint Constructors wfA.

Scheme wfX_wfA_ind := Induction for wfX Sort Prop
 with  wfA_wfX_ind := Induction for wfA Sort Prop.

Combined Scheme wfX_wfA_mutind
 from wfX_wfA_ind, wfA_wfX_ind.


(* Closed expressions are well formed under an empty environment. *)
Definition closedX (xx: exp) : Prop
 := wfX Empty xx.
Hint Unfold closedX.


(* Values are closed expressions that cannot be reduced further. *)
Inductive value : exp -> Prop :=
 | Value 
   :  forall xx
   ,  whnfX xx -> closedX xx
   -> value xx.
Hint Constructors value.



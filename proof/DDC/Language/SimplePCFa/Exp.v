
Require Export DDC.Base.
Require Export DDC.Language.SimplePCFa.Ty.


(* Constants *)
Inductive const : Type := 
  | CBool   : bool -> const
  | CNat    : nat -> const.
Hint Constructors const.


(* Primitive Operators *)
Inductive op1 : Type := 
  | OSucc   : op1
  | OPred   : op1
  | OIsZero : op1.
Hint Constructors op1.


(* Values *)
Inductive val : Type := 
  | VVar    : nat   -> val
  | VConst  : const -> val
  | VLam    : ty    -> exp -> val
  | VFix    : ty    -> val -> val

(* Expressions *)
with     exp : Type :=
  | XVal    : val -> exp
  | XLet    : ty  -> exp -> exp -> exp
  | XApp    : val -> val -> exp
  | XOp1    : op1 -> val -> exp
  | XIf     : val -> exp -> exp -> exp.
Hint Constructors val.
Hint Constructors exp.


(******************************************************************************)
(* Induction principle for expressions. *)
Lemma exp_mutind : forall 
    (PX : exp -> Prop)
    (PV : val -> Prop)
 ,  (forall n,                                   PV (VVar n))
 -> (forall c,                                   PV (VConst c))
 -> (forall t x,      PX x                    -> PV (VLam t x))
 -> (forall t v,      PV v                    -> PV (VFix t v))
 -> (forall v,        PV v                    -> PX (XVal v))
 -> (forall t x1 x2,  PX x1 -> PX x2          -> PX (XLet t x1 x2))
 -> (forall v1 v2,    PV v1 -> PV v2          -> PX (XApp v1 v2))
 -> (forall o v,      PV v                    -> PX (XOp1 o v))
 -> (forall v1 x2 x3, PV v1 -> PX x2 -> PX x3 -> PX (XIf v1 x2 x3))
 ->  forall x, PX x.
Proof. 
 intros PX PV.
 intros hVar hConst hLam hFix hVal hLet hApp hOp1 hIf.
 refine (fix  IHX x : PX x := _
         with IHV v : PV v := _
         for  IHX).

 (* expressions *)
 case x; intros.
 apply hVal. apply IHV.
 apply hLet. apply IHX. apply IHX.
 apply hApp. apply IHV. apply IHV.
 apply hOp1. apply IHV.
 apply hIf.  apply IHV. apply IHX. apply IHX.

 (* values *)
 case v; intros.
 apply hVar.
 apply hConst.
 apply hLam. apply IHX.
 apply hFix. apply IHV.
Qed.


(******************************************************************************)
(* Well formed expressions are closed under the given environment. *)
Inductive wfX (tn: nat) : exp -> Prop :=
 | WfX_VVar
   :  forall ti
   ,  ti < tn
   -> wfX tn (XVal (VVar ti))
 
 | WfX_VConst
   :  forall c
   ,  wfX tn (XVal (VConst c))

 | WfX_Lam
   :  forall t1 x2
   ,  wfX (S tn) x2
   -> wfX tn  (XVal (VLam t1 x2))

 | WfX_Fix
   :  forall t1 v2
   ,  wfX (S tn) (XVal v2)
   -> wfX tn  (XVal (VFix t1 v2))

 | WfX_XLet
   : forall t1 x1 x2
   ,  wfX tn     x1
   -> wfX (S tn) x2
   -> wfX tn     (XLet t1 x1 x2)

 | WfX_XApp 
   :  forall v1 v2
   ,  wfX tn (XVal v1)
   -> wfX tn (XVal v2)
   -> wfX tn (XApp v1 v2)

 | WfX_XOp 
   :  forall o v
   ,  wfX tn (XVal v)
   -> wfX tn (XOp1 o v)

 | WfX_XIf
   :  forall v1 x2 x3
   ,  wfX tn (XVal v1)
   -> wfX tn x2
   -> wfX tn x3
   -> wfX tn (XIf v1 x2 x3).


(* Closed expressions are well formed under an empty environment. *)
Definition closedX (xx: exp) : Prop 
 := wfX 0 xx.


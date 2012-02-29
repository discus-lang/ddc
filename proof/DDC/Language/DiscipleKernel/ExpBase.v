
Require Export DDC.Base.
Require Export DDC.Language.DiscipleKernel.TyExp.


(* Constants *)
Inductive const : Type := 
  | CLoc    : nat      -> const
  | CBool   : bool     -> const
  | CNat    : nat      -> const
  | CArray  : list nat -> const.
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
  | VLAM    : ki    -> exp -> val
  | VFix    : ty    -> val -> val

(* Expressions *)
with     exp : Type :=
  | XVal    : val -> exp
  | XLet    : ty  -> exp -> exp -> exp
  | XSat    : val -> wit -> exp
  | XApp    : val -> val -> exp
  | XAPP    : val -> ty  -> exp
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
 -> (forall k x,      PX x                    -> PV (VLAM k x))
 -> (forall t v,      PV v                    -> PV (VFix t v))
 -> (forall v,        PV v                    -> PX (XVal v))
 -> (forall t x1 x2,  PX x1 -> PX x2          -> PX (XLet t x1 x2))
 -> (forall v w,      PV v                    -> PX (XSat v  w))
 -> (forall v1 v2,    PV v1 -> PV v2          -> PX (XApp v1 v2))
 -> (forall v  t,     PV v                    -> PX (XAPP v  t))
 -> (forall o v,      PV v                    -> PX (XOp1 o v))
 -> (forall v1 x2 x3, PV v1 -> PX x2 -> PX x3 -> PX (XIf v1 x2 x3))
 ->  forall x, PX x.
Proof. 
 intros PX PV.
 intros hVar hConst hLam hLAM hFix hVal hLet hSat hApp hAPP hOp1 hIf.
 refine (fix  IHX x : PX x := _
         with IHV v : PV v := _
         for  IHX).

 (* expressions *)
 case x; intros.
 apply hVal. apply IHV.
 apply hLet. apply IHX. apply IHX.
 apply hSat. apply IHV.
 apply hApp. apply IHV. apply IHV.
 apply hAPP. apply IHV.
 apply hOp1. apply IHV.
 apply hIf.  apply IHV. apply IHX. apply IHX.

 (* values *)
 case v; intros.
 apply hVar.
 apply hConst.
 apply hLam. apply IHX.
 apply hLAM. apply IHX.
 apply hFix. apply IHV.
Qed.


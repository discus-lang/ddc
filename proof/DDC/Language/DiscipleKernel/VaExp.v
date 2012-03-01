
Require Export DDC.Base.
Require Export DDC.Language.DiscipleKernel.TyExp.
Require Export DDC.Language.DiscipleKernel.WiExp.


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

Inductive op2 : Type :=
  | ORead   : op2.
Hint Constructors op2.

Inductive op3 : Type :=
  | OUpdate : op3.
Hint Constructors op3.

(* Values *)
Inductive val : Type := 
  | VVar    : nat   -> val
  | VLam    : ty    -> exp -> val
  | VLAM    : ki    -> exp -> val
  | VFix    : ty    -> val -> val
  | VSat    : val   -> wit -> val
  | VAPP    : val   -> ty  -> val
  | VConst  : const -> val

(* Expressions *)
with     exp : Type :=
  | XVal    : val -> exp
  | XLet    : ty  -> exp -> exp -> exp
  | XApp    : val -> val -> exp
  | XIf     : val -> exp -> exp -> exp
  | XOp1    : op1 -> val -> exp
  | XOp2    : op2 -> val -> val -> exp
  | XOp3    : op3 -> val -> val -> val -> exp.
Hint Constructors val.
Hint Constructors exp.


(******************************************************************************)
(* Induction principle for expressions. *)
Lemma exp_mutind : forall 
    (PX : exp -> Prop)
    (PV : val -> Prop)
 ,  (forall n,                                     PV (VVar n))
 -> (forall t x,        PX x                    -> PV (VLam t x))
 -> (forall k x,        PX x                    -> PV (VLAM k x))
 -> (forall t v,        PV v                    -> PV (VFix t v))
 -> (forall v w,        PV v                    -> PV (VSat v  w))
 -> (forall v  t,       PV v                    -> PV (VAPP v  t))
 -> (forall c,                                     PV (VConst c))
 -> (forall v,          PV v                    -> PX (XVal v))
 -> (forall t x1 x2,    PX x1 -> PX x2          -> PX (XLet t x1 x2))
 -> (forall v1 v2,      PV v1 -> PV v2          -> PX (XApp v1 v2))
 -> (forall v1 x2 x3,   PV v1 -> PX x2 -> PX x3 -> PX (XIf v1 x2 x3))
 -> (forall o v,        PV v                    -> PX (XOp1 o v))
 -> (forall o v1 v2,    PV v1 -> PV v2          -> PX (XOp2 o v1 v2))
 -> (forall o v1 v2 v3, PV v1 -> PV v2 -> PV v3 -> PX (XOp3 o v1 v2 v3))
 ->  forall x, PX x.
Proof. 
 intros PX PV.
 intros hVar hLam hLAM hFix hSat hAPP hConst hVal hLet hApp hIf hOp1 hOp2 hOp3.
 refine (fix  IHX x : PX x := _
         with IHV v : PV v := _
         for  IHX).

 (* expressions *)
 case x; intros.
 apply hVal. apply IHV.
 apply hLet. apply IHX. apply IHX.
 apply hApp. apply IHV. apply IHV.
 apply hIf.  apply IHV. apply IHX. apply IHX.
 apply hOp1. apply IHV.
 apply hOp2. apply IHV. apply IHV.
 apply hOp3. apply IHV. apply IHV. apply IHV.

 (* values *)
 case v; intros.
 apply hVar.
 apply hLam. apply IHX.
 apply hLAM. apply IHX.
 apply hFix. apply IHV.
 apply hSat. apply IHV.
 apply hAPP. apply IHV.
 apply hConst.
Qed.


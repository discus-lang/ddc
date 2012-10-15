
Require Export DDC.Base.
Require Export DDC.Language.SystemF2Effect.TyExp.


(* Constants *)
Inductive const : Type := 
  | CLoc    : nat      -> const
  | CNat    : nat      -> const
  | CBool   : bool     -> const.
Hint Constructors const.


(* Primitive Operators *)
Inductive op1 : Type := 
  | OSucc   : op1
  | OPred   : op1
  | OIsZero : op1
  | ORead   : op1.
Hint Constructors op1.

Inductive op2 : Type := 
  | OWrite  : op2.
Hint Constructors op2.

(* Values *)
Inductive val : Type := 
  | VVar    : nat   -> val
  | VLam    : ty    -> exp -> val
  | VLAM    : ki    -> exp -> val
  | VAPP    : val   -> ty  -> val
  | VConst  : const -> val

(* Expressions *)
with     exp : Type :=
  | XVal    : val -> exp
  | XLet    : ty  -> exp -> exp -> exp
  | XApp    : val -> val -> exp
  | XOp1    : op1 -> val -> exp
  | XOp2    : op2 -> val -> val -> exp.
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
 -> (forall v  t,       PV v                    -> PV (VAPP v  t))
 -> (forall c,                                     PV (VConst c))
 -> (forall v,          PV v                    -> PX (XVal v))
 -> (forall t x1 x2,    PX x1 -> PX x2          -> PX (XLet t x1 x2))
 -> (forall v1 v2,      PV v1 -> PV v2          -> PX (XApp v1 v2))
 -> (forall o v,        PV v                    -> PX (XOp1 o v))
 -> (forall o v1 v2,    PV v1 -> PV v2          -> PX (XOp2 o v1 v2))
 ->  forall x, PX x.
Proof. 
 intros PX PV.
 intros hVar hLam hLAM hAPP hConst hVal hLet hApp hOp1 hOp2.
 refine (fix  IHX x : PX x := _
         with IHV v : PV v := _
         for  IHX).

 (* expressions *)
 case x; intros.
 apply hVal. apply IHV.
 apply hLet. apply IHX. apply IHX.
 apply hApp. apply IHV. apply IHV.
 apply hOp1. apply IHV.
 apply hOp2. apply IHV. apply IHV.

 (* values *)
 case v; intros.
 apply hVar.
 apply hLam. apply IHX.
 apply hLAM. apply IHX.
 apply hAPP. apply IHV.
 apply hConst.
Qed.


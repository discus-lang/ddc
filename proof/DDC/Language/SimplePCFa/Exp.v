
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



Require Export DDC.Base.
Require Export DDC.Language.SimplePCFa.Ty.


(* Constants *)
Inductive const : Type := 
  | CTrue   : const
  | CFalse  : const
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
  | VFun    : ty    -> exp -> val

(* Expressions *)
with     exp : Type :=
  | XVal    : val -> exp
  | XLet    : ty  -> exp -> exp
  | XOp1    : op1 -> val -> exp
  | XIf     : val -> exp -> exp -> exp.
Hint Constructors val.
Hint Constructors exp.


(* Lifting of references into the environment *)
Fixpoint liftXV (d: nat) (vv: val) : val := 
  match vv with
  | VVar i
  => if le_gt_dec d i
       then VVar (S i)
       else vv
  | VConst c     => VConst c
  | VFun t x     => VFun t (liftXX (S d) x)
  end

with    liftXX (d: nat) (xx: exp) : exp :=
  match xx with
  | XVal v       => XVal (liftXV d v)
  | XOp1 o v     => XOp1 o (liftXV d v)
  | XLet t x     => XLet t (liftXX (S d) x)
  | XIf v1 x2 x3 => XIf  (liftXV d v1) (liftXX d x2) (liftXX d x3)
  end.


(* Substitute for the outer-most binder in an expression *)
Fixpoint substVV (d: nat) (u: val) (vv: val) :=
  match vv with
  | VVar ix 
  => match nat_compare ix d with
     | Eq  => u
     | Gt  => VVar (ix - 1)
     | Lt  => VVar ix
     end
  | VConst c     => VConst c
  | VFun t x     => VFun t (substVX (S d) (liftXV 0 u) x)
  end

with    substVX (d: nat) (u: val) (xx: exp) :=
  match xx with
  | XVal v       => XVal (substVV d u v)
  | XOp1 o v     => XOp1 o (substVV d u v)
  | XLet t x     => XLet t (substVX d u x)
  | XIf v1 x2 x3 => XIf  (substVV d u v1) (substVX d u x2) (substVX d u x3)
  end.


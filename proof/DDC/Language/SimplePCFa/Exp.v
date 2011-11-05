
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
  | VFun    : ty    -> exp -> val

(* Expressions *)
(* All evaluation is forced by the let expression. *)
with     exp : Type :=
  | XVal    : val -> exp
  | XLet    : ty  -> exp -> exp -> exp
  | XApp    : val -> val -> exp
  | XOp1    : op1 -> val -> exp
  | XIf     : val -> exp -> exp -> exp.
Hint Constructors val.
Hint Constructors exp.




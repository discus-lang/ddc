
Require Export Base.
Require Export Env.


(** Expressions *****************************************************)
Inductive exp : Type :=
 | XVar  : nat -> exp
 | XLam  : ty  -> exp -> exp
 | XApp  : exp -> exp -> exp.
Hint Constructors exp.


(** Environments ****************************************************)
Definition tyenv := env ty.

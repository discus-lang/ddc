
Require Export Env.


(* Kinds ************************************************************)
Inductive ki : Type :=
 | KStar   : ki.

Definition kienv := env ki.
Hint Unfold kienv.


(** Types ***********************************************************)
Inductive ty  : Type :=
 | TCon    : nat -> ty
 | TVar    : nat -> ty
 | TForall : ty  -> ty
 | TFun    : ty  -> ty -> ty.
Hint Constructors ty.

Definition tyenv := env ty.
Hint Unfold tyenv.


(** Expressions *****************************************************)
Inductive exp : Type :=
 | XVar  : nat -> exp
 | XLAM  : exp -> exp
 | XAPP  : exp -> ty  -> exp
 | XLam  : ty  -> exp -> exp
 | XApp  : exp -> exp -> exp.
Hint Constructors exp.



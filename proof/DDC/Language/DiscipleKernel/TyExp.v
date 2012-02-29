
Require Export DDC.Language.DiscipleKernel.Ki.
Require Export DDC.Language.DiscipleKernel.TyCon.


(********************************************************************)
(* Type Expressions. *)
Inductive ty  : Type :=
 | TVar      : nat   -> ty
 | TCon      : tycon -> ty
 | TForall   : ki    -> ty -> ty
 | TApp      : ty    -> ty -> ty
 | TSum      : ty    -> ty -> ty
 | TBot      : ki    -> ty.
Hint Constructors ty.


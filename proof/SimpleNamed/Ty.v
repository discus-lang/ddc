
Require Export Name.

(* types **************************************************)
Inductive ty : Type :=
 | TVar  : name -> ty
 | TFun  : ty -> ty -> ty.


Notation tA := (TVar (Name 0)).
Notation tB := (TVar (Name 1)).
Notation tC := (TVar (Name 2)).

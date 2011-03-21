
Require Import Base.
Require Import Name.

 

(* types **************************************************)
Inductive ty : Type :=
 | TVar  : name -> ty
 | TFun  : ty -> ty -> ty.

Notation A := (TVar (Name 0)).
Notation B := (TVar (Name 1)).
Notation C := (TVar (Name 2)).


(* terms **************************************************)
Inductive exp : Type :=
 | XVar  : name -> exp
 | XApp  : exp  -> exp -> exp
 | XLam  : name -> ty  -> exp -> exp.











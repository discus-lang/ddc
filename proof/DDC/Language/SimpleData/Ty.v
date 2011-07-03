

(* Type Constructors *)
Inductive tycon : Type :=
 | TyConData   : nat -> tycon.
Hint Constructors tycon.


(* Types *)
Inductive ty : Type :=
 | TCon   : tycon -> ty
 | TFun   : ty    -> ty -> ty.
Hint Constructors ty.


(* Type Environment *)
Definition tyenv := list ty.



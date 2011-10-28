
Require Export DDC.Base.


(* Type Constructors *)
Inductive tycon : Type :=
 | TyConBool  : tycon
 | TyConNat   : tycon.
Hint Constructors tycon.


(* Type Expressions *)
Inductive ty  : Type :=
 | TCon       : tycon -> ty
 | TFun       : ty    -> ty -> ty.
Hint Constructors ty.


(* Baked-in Types *)
Definition tBool := TCon TyConBool.
Definition tNat  := TCon TyConNat.


(* Type Environments *)
Definition tyenv := list ty.


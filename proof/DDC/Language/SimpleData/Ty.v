
Require Import DDC.Base.

(* Type Constructors *)
Inductive tycon : Type :=
 | TyConData   : nat -> tycon.
Hint Constructors tycon.


Fixpoint tycon_beq t1 t2 :=
  match t1, t2 with
  | TyConData n1, TyConData n2 => beq_nat n1 n2
  end.


(* Types *)
Inductive ty : Type :=
 | TCon   : tycon -> ty
 | TFun   : ty    -> ty -> ty.
Hint Constructors ty.



(* Type Environment *)
Definition tyenv := list ty.




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


(* Baked-in Witness Types *)
Definition tImpl  t1 t2         := TApp (TApp (TCon TyConImpl) t1) t2.
Definition tPure    e1          := TApp (TCon TyConPure)    e1.
Definition tEmpty   c1          := TApp (TCon TyConEmpty)   c1.
Definition tConst   r1          := TApp (TCon TyConConst)   r1.
Definition tMutable r1          := TApp (TCon TyConMutable) r1. 


(* Baked-in Effect Types *)
Definition tRead    r1          := TApp (TCon TyConRead)  r1.
Definition tWrite   r1          := TApp (TCon TyConWrite) r1.
Definition tAlloc   r1          := TApp (TCon TyConAlloc) r1.


(* Baked-in Closure Types *)
Definition tUse     r1          := TApp (TCon TyConUse) r1.


(* Baked-in Value Types *)
Definition tFun   t1 eff clo t2 := TApp (TApp (TApp (TApp (TCon TyConFun) t1) eff) clo) t2.
Definition tBool                := TCon TyConBool.
Definition tNat                 := TCon TyConNat.
Definition tArray r1            := TApp (TCon TyConArray) r1.


(* Type Environments *)
Definition tyenv := list ty.

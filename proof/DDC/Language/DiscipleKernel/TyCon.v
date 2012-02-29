
Require Export DDC.Language.DiscipleKernel.Ki.


(* Type Constructors. *)
Inductive tycon : Type :=
 | TyConImpl : tycon
 | TyConFun  : tycon
 | TyConData : nat   -> ki -> tycon.
Hint Constructors tycon.


Fixpoint tycon_beq t1 t2 :=
  match t1, t2 with
  | TyConImpl,      TyConImpl      => true
  | TyConFun,       TyConFun       => true
  | TyConData n1 _, TyConData n2 _ => beq_nat n1 n2
  | _,              _              => false
  end.


Definition isTyConFun  (tc: tycon) : Prop :=
  match tc with
  | TyConImpl     => False
  | TyConFun      => True
  | TyConData _ _ => False
  end.
Hint Unfold isTyConFun.


Definition isTyConData (tc: tycon) : Prop :=
  match tc with
  | TyConImpl     => False
  | TyConFun      => False
  | TyConData _ _ => True
  end.
Hint Unfold isTyConData.


Require Export DDC.Language.DiscipleKernel.Ki.


(* Type Constructors. *)
Inductive tycon : Type :=
  | TyConImpl    : tycon
  | TyConFun     : tycon

  (* Witness Types *)
  | TyConConst   : tycon
  | TyConMutable : tycon
  | TyConPure    : tycon
  | TyConEmpty   : tycon

  (* Effect Types *)
  | TyConRead    : tycon
  | TyConWrite   : tycon
  | TyConAlloc   : tycon

  (* Closure Types *)
  | TyConUse     : tycon

  (* Value Types *)
  | TyConBool    : tycon
  | TyConNat     : tycon
  | TyConArray   : tycon
  | TyConData    : nat   -> ki -> tycon.
Hint Constructors tycon.


Fixpoint tycon_beq t1 t2 :=
  match t1, t2 with
  | TyConImpl,      TyConImpl      => true
  | TyConFun,       TyConFun       => true
  | TyConConst,     TyConConst     => true
  | TyConMutable,   TyConMutable   => true
  | TyConPure,      TyConPure      => true
  | TyConEmpty,     TyConEmpty     => true
  | TyConRead,      TyConRead      => true
  | TyConWrite,     TyConWrite     => true
  | TyConAlloc,     TyConAlloc     => true
  | TyConUse,       TyConUse       => true
  | TyConBool,      TyConBool      => true
  | TyConNat,       TyConNat       => true
  | TyConArray,     TyConArray     => true
  | TyConData n1 _, TyConData n2 _ => beq_nat n1 n2
  | _,              _              => false
  end.


Definition isTyConFun  (tc: tycon) : Prop :=
  match tc with
  | TyConFun      => True
  | _             => False
  end.
Hint Unfold isTyConFun.


Definition isTyConData (tc: tycon) : Prop :=
  match tc with
  | TyConData _ _ => True
  | _             => False
  end.
Hint Unfold isTyConData.



Require Export DDC.Language.SystemF2Effect.Ki.


(* Type Constructors. *)
Inductive tycon : Type :=

  (* Value type constructors *)
  | TyConFun   : tycon
  | TyConUnit  : tycon
  | TyConBool  : tycon
  | TyConNat   : tycon
  | TyConRef   : tycon
  | TyConData  : nat   -> ki -> tycon

  (* Effect type constructors *)
  | TyConRead  : tycon
  | TyConWrite : tycon
  | TyConAlloc : tycon.
Hint Constructors tycon.


Fixpoint tycon_beq t1 t2 :=
  match t1, t2 with
  | TyConFun,       TyConFun       => true
  | TyConUnit,      TyConUnit      => true
  | TyConBool,      TyConBool      => true
  | TyConNat,       TyConNat       => true
  | TyConRef,       TyConRef       => true
  | TyConData n1 _, TyConData n2 _ => beq_nat n1 n2

  | TyConRead,      TyConRead      => true
  | TyConWrite,     TyConWrite     => true
  | TyConAlloc,     TyConAlloc     => true
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



Require Export DDC.Base.
Require Export DDC.Language.DiscipleKernel.TyExp.


(* Capabilities *)
Inductive cap : Type :=
 | ZMutable  : nat -> cap
 | ZConst    : nat -> cap
 | ZDistinct : list nat -> cap.


(* Witness constructors *)
Inductive wicon : Type :=
 | IPure     : wicon
 | IEmpty    : wicon
 | IRead     : wicon
 | IAlloc    : wicon
 | ICut      : list nat -> wicon.


(* Witness Expressions *)
Inductive wit : Type :=
 | WVar      : nat    -> wit
 | WApp      : wit    -> wit -> wit
 | WAPP      : wit    -> ty  -> wit
 | WJoin     : wit    -> wit -> wit
 | WCap      : cap    -> wit
 | WCon      : wicon  -> wit.
Hint Constructors wit.


Definition wPure  e1    := WAPP (WCon IPure)  e1.
Definition wEmpty c1    := WAPP (WCon IEmpty) c1.
Definition wRead  r  w  := WApp (WAPP (WCon IRead)  r) w.
Definition wAlloc r  w  := WApp (WAPP (WCon IAlloc) r) w.


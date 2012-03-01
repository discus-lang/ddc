

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
 | IRead     : wicon
 | ICut      : list nat -> wicon.


(* Witness Expressions *)
Inductive wit : Type :=
 | WVar      : nat    -> wit
 | WCap      : cap    -> wit
 | WCon      : wicon  -> wit
 | WApp      : wit    -> wit -> wit
 | WAPP      : wit    -> ty  -> wit
 | WJoin     : wit    -> wit -> wit.
Hint Constructors wit.


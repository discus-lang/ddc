
Require Export DDC.Base.

(* Sorts *)
Inductive so : Type :=
  | SProp    : so
  | SComp    : so.
Hint Constructors so.


(* Kinds *)
Inductive ki : Type :=
  | KData    : ki
  | KRegion  : ki
  | KEffect  : ki
  | KClosure : ki
  | KWitness : ki
  | KFun     : ki -> ki -> ki.
Hint Constructors ki.


(* Kind Environments *)
Definition kienv := list ki.
Hint Unfold kienv.



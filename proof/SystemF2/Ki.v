
Require Export Env.
Require Export Base.


(* Kinds ************************************************************)
Inductive ki : Type :=
 | KStar   : ki
 | KFun    : ki -> ki -> ki.
Hint Constructors ki.


Definition kienv := env ki.
Hint Unfold kienv.




Require Export Env.
Require Export Base.


(* Kinds ************************************************************)
Inductive ki : Type :=
 | KStar   : ki.

Definition kienv := env ki.
Hint Unfold kienv.



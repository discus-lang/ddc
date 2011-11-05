
Require Import DDC.Language.SimplePCFa.Exp.


(* Frame stacks *)
Inductive frame : Type :=
 | FNil   : frame
 | FSnoc  : frame -> exp -> frame.
Hint Constructors frame.



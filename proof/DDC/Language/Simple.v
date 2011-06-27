(* Simply Typed Lambda Calculus (STLC) 
   Uses deBruijn indices for name binding. *)

(* Types, expressions, normal forms, values, lifting and substitution *)
Require Export DDC.Language.Simple.Exp.

(* Typing judgement and environment weakening. *)
Require Export DDC.Language.Simple.Ty.

(* Substitution of exps in exps preserves typing. *)
Require Export DDC.Language.Simple.SubstExpExp.

(* Small step evaluation. *)
Require Export DDC.Language.Simple.Step.

(* A well typed expression is either a value or can take a step. *)
Require Export DDC.Language.Simple.Progress.

(* When an expression takes a step then the result has the same type. *)
Require Export DDC.Language.Simple.Preservation.

(* Big step evaluation, and conversion between small step evaluation. *)
Require Export DDC.Language.Simple.Eval.

(* Bonus lemmas not used by the other theorems. *)
Require Export DDC.Language.Simple.Bonus.

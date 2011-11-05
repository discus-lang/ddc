
(* Types, expressions, normal forms, values, lifting and substitution *)
Require Export DDC.Language.SimplePCFa.Exp.
Require Export DDC.Language.SimplePCFa.ExpLift.
Require Export DDC.Language.SimplePCFa.ExpSubst.

(* Type Expressions *)
Require Export DDC.Language.SimplePCFa.Ty.

(* Type Judgement *)
Require Export DDC.Language.SimplePCFa.TyJudge.

(*
(* Substitution of exps in exps preserves typing. *)
Require Export DDC.Language.SimplePCF.SubstExpExp.
*)
(* Small step evaluation. *)
Require Export DDC.Language.SimplePCFa.Step.

(*
(* A well typed expression is either a value or can take a step. *)
Require Export DDC.Language.SimplePCF.Progress.

(* When an expression takes a step then the result has the same type. *)
Require Export DDC.Language.SimplePCF.Preservation.

(* Big step evaluation, and conversion between small step evaluation. *)
Require Export DDC.Language.SimplePCF.Eval.
*)
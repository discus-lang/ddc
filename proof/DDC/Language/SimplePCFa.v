
(* Types, expressions, normal forms, values, lifting and substitution *)
Require Export DDC.Language.SimplePCFa.Exp.
Require Export DDC.Language.SimplePCFa.ExpRefs.
Require Export DDC.Language.SimplePCFa.ExpLift.
Require Export DDC.Language.SimplePCFa.ExpLower.
Require Export DDC.Language.SimplePCFa.ExpSwap.
Require Export DDC.Language.SimplePCFa.ExpSubst.
Require Export DDC.Language.SimplePCFa.ExpSubsts.

(* Type Expressions *)
Require Export DDC.Language.SimplePCFa.Ty.

(* Type Judgement *)
Require Export DDC.Language.SimplePCFa.TyJudge.

(*
(* Substitution of exps in exps preserves typing. *)
Require Export DDC.Language.SimplePCF.SubstExpExp.
*)
(* Small step evaluation. *)
Require Export DDC.Language.SimplePCFa.StepBase.
Require Export DDC.Language.SimplePCFa.StepFrame.
Require Export DDC.Language.SimplePCFa.StepTerm.


(* Big step evaluation, and conversion between small step evaluation. *)
Require Export DDC.Language.SimplePCFa.Eval.

(* CIU equivalence *)
(* Require Export DDC.Language.SimplePCFa.EquivCIU. *)

(*
(* A well typed expression is either a value or can take a step. *)
Require Export DDC.Language.SimplePCF.Progress.

(* When an expression takes a step then the result has the same type. *)
Require Export DDC.Language.SimplePCF.Preservation.

*)
(* System-F.
   Like STLC, but with type abstraction and application. *)

(* Kinds and kind environemnts. *)
Require Export DDC.Language.SystemF.Ki.

(* Types, well formed and closed, lifting and substitution lemmas. *)
Require Export DDC.Language.SystemF.Ty.

(* Type environments, lifting and substitution lemmas. *)
Require Export DDC.Language.SystemF.TyEnv.

(* Expressions, normal forms, lifting and substitution. *)
Require Export DDC.Language.SystemF.Exp.

(* Kinds of types, weakening the kind environment. *)
Require Export DDC.Language.SystemF.KiJudge.

(* Substitution of types in types preserves kinding. *)
Require Export DDC.Language.SystemF.SubstTypeType.

(* Type Judgement. *)
Require Export DDC.Language.SystemF.TyJudge.

(* Substitution of types in expressions preserves typing. *)
Require Export DDC.Language.SystemF.SubstTypeExp.

(* Substitution of expressions in expressions preserves typing. *)
Require Export DDC.Language.SystemF.SubstExpExp.

(* Small step evaluation. *)
Require Export DDC.Language.SystemF.Step.

(* A well typed expression is either a value, or can take a step. *)
Require Export DDC.Language.SystemF.Progress.

(* When an expression takes a step the results has the same type. *)
Require Export DDC.Language.SystemF.Preservation.

(* Big step evaluation, and conversion to small steps. *)
Require Export DDC.Language.SystemF.Eval.


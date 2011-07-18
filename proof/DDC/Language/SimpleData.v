
(* STLC with algebraic data types. *)

(* Types, well formed and closed, lifting and substitution lemmas. *)
Require Import DDC.Language.SimpleData.Ty.

(* Type definitions *)
Require Import DDC.Language.SimpleData.Def.

(* Expression structure *)
Require Import DDC.Language.SimpleData.ExpBase.

(* Lifting of debruijn indices in expressions. *)
Require Import DDC.Language.SimpleData.ExpLift.

(* Substitution for expressions. *)
Require Import DDC.Language.SimpleData.ExpSubst.

(* Functions and lemmas concerning case alternatives. *)
Require Import DDC.Language.SimpleData.ExpAlt.

(* Tie the above together, weak normal formes, closedX and value *)
Require Import DDC.Language.SimpleData.Exp.

(* Type judgement assigns a type to an expression. *)
Require Import DDC.Language.SimpleData.TyJudge.

(* Substitution of expressions in expressions preserves typing. *)
Require Import DDC.Language.SimpleData.SubstExpExp.

(* Evaluation contexts *)
Require Import DDC.Language.SimpleData.StepContext.

(* Lemmas for evaluation of case alternatives. *)
Require Import DDC.Language.SimpleData.StepAlt.

(* Single step evaluation rules *)
Require Import DDC.Language.SimpleData.Step.

(* A well typed expression is a value or can take a step*)
Require Import DDC.Language.SimpleData.Progress.

(* When an expression takes a step the result has the same type as before *)
Require Import DDC.Language.SimpleData.Preservation.

(* Construction of contexts for big step evaluation *) 
Require Import DDC.Language.SimpleData.EvalChain.

(* Big step evaluation and conversion between small steps *)
Require Import DDC.Language.SimpleData.Eval.


(* Top level module imports everything so that we have a
   nice make target to build the proofs. *)
Require Import DDC.Base.

(* Simply Typed Lambda Calculus (STLC) *)
Require Import DDC.Language.Simple.

(* STLC with Fixpoints, Boolean choice and Naturals. *)
Require Import DDC.Language.SimplePCF.

(* System-F. Like STLC, but with type abstraction and application. *)
Require Import DDC.Language.SystemF.



Require Import DDC.Language.SystemF2Effect.TyJudge.
Require Import DDC.Language.SystemF2Effect.VaExpBase.


(********************************************************************)
Definition store := list val.


(********************************************************************)
(* Store typing models the store.
   All types in the store typing have a corresponding binding in
   the store *)
Definition STOREM (se: stenv) (ss: store)
 := length se = length ss.
Hint Unfold STOREM.


(********************************************************************)
(* Well typed store. *)
Definition STORET (se: stenv) (ss: store)
 := Forall2 (TYPEV nil nil se) ss se.
Hint Unfold STORET.


(*******************************************************************)
(* Well formed store. *)
Definition WfS (se: stenv) (ss: store)
 := STOREM se ss
 /\ STORET se ss.



Require Export DDC.Language.SystemF2Store.TyJudge.
Require Export DDC.Language.SystemF2Store.Exp.

(********************************************************************)
(* Storeable values are the ones that we can keep directly in 
   store bindings *)
Inductive svalue :=
 | SLoc  : nat -> svalue
 | SLAM  : exp -> svalue
 | SLam  : ty  -> exp -> svalue.
Hint Constructors svalue.


Definition takeSValueOfExp (xx : exp) : option svalue :=
 match xx with
 | XLoc n    => Some (SLoc n)
 | XLAM x    => Some (SLAM x)
 | XLam t x  => Some (SLam t x)
 | _         => None
 end.

Definition expOfSValue (s: svalue) : exp :=
 match s with
 | SLoc n    => XLoc n
 | SLAM x    => XLAM x
 | SLam t x  => XLam t x
 end.

Definition svalueOf (xx : exp) (sv : svalue) : Prop
 := takeSValueOfExp xx = Some sv.


(* Store binding with a constructor tag and some storeable values *)
Inductive sbind :=
 | SObj : datacon -> list svalue -> sbind.


Definition store  := list sbind.
Hint Unfold store.

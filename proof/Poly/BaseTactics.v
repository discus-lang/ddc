
Require Export Omega.
Require Export BaseLibTactics.
Require Import Coq.Arith.Compare_dec.


(* Tactics ************************************************)
Tactic Notation "break" constr(E) :=
 let X := fresh "X" in remember (E) as X; destruct X.

Tactic Notation "breaka" constr(E) :=
 let X := fresh "X" in remember (E) as X; destruct X; auto.



(* Tactics **********************************************************)
(* Break an of the form (compare ?E1 ?E2) into the possible orderings
      and substitute the ?E1 = ?E2 when they are equal.
*)
Tactic Notation "break_nat_compare" constr(E1) constr(E2) :=
 let X := fresh "X" 
 in  remember (nat_compare E1 E2) as X; destruct X;     
      [ match goal with 
         |  [ H: Eq = nat_compare E1 E2 |- _ ] 
         => symmetry in H; apply nat_compare_eq in H; subst
        end
      | match goal with 
         |  [ H: Lt = nat_compare E1 E2 |- _ ]
         => symmetry in H; apply nat_compare_lt in H
        end 
      | match goal with
         |  [ H: Gt = nat_compare E1 E2 |- _ ]
         => symmetry in H; apply nat_compare_gt in H
        end
      ].


(* Find an expression of the form (compare ?E1 ?E2)
 	and call break_compare on it. 
*)
Tactic Notation "fbreak_nat_compare" := 
 match goal with 
  | [ |- context[nat_compare ?E1 ?E2] ] 
    => break_nat_compare E1 E2
 end.

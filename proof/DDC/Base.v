
Require Export DDC.Data.List.
Require Export DDC.Data.Nat.
Require Export DDC.Base.Tactics.
Require Export Coq.Program.Basics.


(* Cases library due to Aaron Bohannon ********************)
Require String. Open Scope string_scope.

Ltac move_to_top x :=
  match reverse goal with
  | H : _ |- _ => try move x after H
  end.

Tactic Notation "assert_eq" ident(x) constr(v) :=
  let H := fresh in
  assert (x = v) as H by reflexivity;
  clear H.

Tactic Notation "Case_aux" ident(x) constr(name) :=
  first [
    set (x := name); move_to_top x
  | assert_eq x name; move_to_top x
  | fail 1 "because we are working on a different case" ].

Tactic Notation "Case" constr(name) := Case_aux Case name.
Tactic Notation "SCase" constr(name) := Case_aux SCase name.
Tactic Notation "SSCase" constr(name) := Case_aux SSCase name.
Tactic Notation "SSSCase" constr(name) := Case_aux SSSCase name.
Tactic Notation "SSSSCase" constr(name) := Case_aux SSSSCase name.
Tactic Notation "SSSSSCase" constr(name) := Case_aux SSSSSCase name.
Tactic Notation "SSSSSSCase" constr(name) := Case_aux SSSSSSCase name.
Tactic Notation "SSSSSSSCase" constr(name) := Case_aux SSSSSSSCase name.


(********************************************************************)
(* Tactic to help deal with lifting functions *)
Ltac fbreak_get 
 := match goal with 
     |  [ |- context [get ?E1 ?E2] ] 
     => let X := fresh 
        in remember (get E1 E2) as X; destruct X
    end.


Ltac fbreak_le_gt_dec
 := match goal with 
     |  [ |- context [le_gt_dec ?n ?n'] ]
     => let X := fresh 
        in remember (le_gt_dec n n') as X; destruct X
    end.


Ltac lift_cases := 
 repeat (intros;
  first [ fbreak_nat_compare
        | fbreak_le_gt_dec
        | fbreak_get]); intros.


Ltac lift_burn t
 := induction t; intros; eauto;  

          (* this gets most var cases *)
    first [ repeat (simpl; lift_cases; nnat; intros); burn 
      
          (* try to apply rewrites from the hypotheses *)    
          | repeat rewritess ].



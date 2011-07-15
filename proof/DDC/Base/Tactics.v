
Require Export DDC.Base.LibTactics.
Require Export Omega.

(********************************************************************)
(* Shorthands for existing tactics *)

Tactic Notation "int"
 := intuition.

Tactic Notation "iauto"
 := intuition eauto with *.

Tactic Notation "spec" hyp(H1) hyp(H2) 
 := specializes H1 H2.
Tactic Notation "spec" hyp(H1) hyp(H2) hyp(H3)
 := specializes H1 H2 H3.
Tactic Notation "spec" hyp(H1) hyp(H2) hyp(H3) hyp(H4)
 := specializes H1 H2 H3 H4.


Tactic Notation "break" constr(E) :=
 let X := fresh "X" in remember (E) as X; destruct X.

Tactic Notation "breaka" constr(E) :=
 let X := fresh "X" in remember (E) as X; destruct X; auto.


(********************************************************************)
(* Breaking up nat_compare
   Find the first (nat_compare ?E1 ?E2) and destruct it into the
   possible orderings. Also substitute ?E1 = ?E2 when they are equal. 
 *)
Ltac fbreak_nat_compare :=
 match goal with 
 |  [ |- context [nat_compare ?E1 ?E2] ]
 => let X := fresh "X" 
    in  remember (nat_compare E1 E2) as X; destruct X;     

        (* In the equality case, sometimes we get equations like
           n = S n, which can't be substituted. Hence try subst. *)
        [ match goal with 
          |  [ H: Eq = nat_compare E1 E2 |- _ ] 
          => symmetry in H; apply nat_compare_eq in H; 
             try subst 
          end

        | match goal with 
          |  [ H: Lt = nat_compare E1 E2 |- _ ]
          => symmetry in H; apply nat_compare_lt in H
          end 

        | match goal with
          |  [ H: Gt = nat_compare E1 E2 |- _ ]
          => symmetry in H; apply nat_compare_gt in H
         end
        ]
 end.


(* Tactic to help deal with lifting functions *)
Ltac lift_cases 
 := match goal with 
     |  [ |- context [le_gt_dec ?n ?n'] ]
     => case (le_gt_dec n n')
    end.

(********************************************************************)
(* Tactics for working with existentials. *)

(* Destruct the existential in hypothesis H,
   and just use the name of the quantifier for the new variable. *)
Ltac dest H
 := match goal with 
    [ H : exists a, _ |- _ ]
     => destruct H as [a]
    end.

Ltac dests H
 := repeat (dest H).


(* Destruct the existential in hypothesis H, 
   and instantiate the existential in the goal with this variable. *)
Ltac shift H
 := match goal with 
    [ H : exists a, _ |- exists b, _] 
     => destruct H as [a]; exists a
    end.

Ltac shifts H
 := repeat (shift H).


(********************************************************************)
(* A better 'false'. 
   Try to eliminate the goal by finding a false hypothesis.
 *)
Ltac nope1
 := match goal with
    (* An equality might be false, so check it before
       attemptiong to clear it in the next case. *)
      [ H : _ = _ |- _] => solve [false]
   
    (* Inverting an equality doesn't make progress, 
       so just get rid of it. *)
    | [ H : _ = _ |- _] => clear H

    (* Keep inverting hypothesis provided we don't get anymore
       goals. If we get more goals then we'll diverge, and we're
       looking to eliminate this goal, not make more. *)
    | [ H : _     |- _] 
      => first [ solve [false]
               | solve [inverts H]
               | (inverts H ; [idtac]) ]
    end.

Ltac nope := repeat nope1.


(********************************************************************)
(* Mega-tactic for semantics proofs.
   * This gets lots of the common cases, where Coq really should 
     have tried a bit harder. For example, trying f_equal before eauto.

   * It also contains cases for patterns that often arise when using
     the deBruijn representation. For example, trying false before
     omega to show that a case concerning index manipulation
     cannot happen.   

   * It does not contain any support specific to a particular 
     language though.
 *)

(* Try primitive tactics from quickest to slowest. 
   These should all fail if they do not solve the goal. *)
Ltac burn0
 := first
    [ assumption       (* Goal is one of the assumptions. *)
    | reflexivity      (* Goal has form e = e. *)
    | omega            (* Solves inequalities with Presburger arithmetic. 
                          Common with deBruijn representations. *)
    | solve [auto]     (* Wrapping these tactics in solve ensures that *)
    | solve [eauto] ]. (*   they fail if they do not apply. *)


(* Try to use injectivity between two constructor applications.
   These are common when we lookup values from the environment, 
    eg with  get ix tyenv = Some t1. *)
Ltac injectos :=
 match goal with 
    [ H1 : _ = ?C ?y1
    , H2 : _ = ?C ?y2 |- _]
     => assert (y1 = y2); rewrite H1 in H2; inverts H2; burn0

  | [ H1 : ?C ?y1 = _
    , H2 : ?C ?y2 = _ |- _]
     => assert (y1 = y2); rewrite H1 in H2; inverts H2; burn0
 end.


(* Try to change the goal in some other way before applying
   one of the primitive tactics. *)
Ltac burn1 
 := first
    [ injectos
    | false;   burn0 
    | f_equal; burn0 ].


(* Top-level megatactic.
   Handles disjunctions by applying burn1 to each of the parts.
    This often happens in progress proofs. eg  value x \/ step x x'. *)
Ltac burn :=
 match goal with 
     [ _ : _ |- _ \/ _ ] 
       => try burn1; try (left; burn1); try (right; burn1)

   | [ H : _ /\ _ |- _]
       => decompose [and] H; burn1

   | _ => burn1
 end.


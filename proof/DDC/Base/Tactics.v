
Require Export DDC.Base.LibTactics.
Require Export Omega.


Tactic Notation "break" constr(E) :=
 let X := fresh "X" in remember (E) as X; destruct X.

Tactic Notation "breaka" constr(E) :=
 let X := fresh "X" in remember (E) as X; destruct X; auto.


(* Find the first (nat_compare ?E1 ?E2) and destruct it into the
   possible orderings. Also substitute ?E1 = ?E2 when they are equal. *)
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
    This often happens in progress proofs. eg  value x \/ step x x'.

   TODO: handle conjuctions common in preservation proofs.*)
Ltac burn :=
 match goal with 
   [ _ : _ |- _ \/ _ ] 
     => try burn1; try (left; burn1); try (right; burn1)

   | _ => burn1
 end.



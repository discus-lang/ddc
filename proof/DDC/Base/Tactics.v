
Require Export DDC.Base.LibTactics.
Require Export Omega.
Require Import Coq.Lists.List.


(********************************************************************)
(* Shorthands for existing tactics *)

Tactic Notation "spec" hyp(H1) hyp(H2) 
 := specializes H1 H2.
Tactic Notation "spec" hyp(H1) hyp(H2) hyp(H3)
 := specializes H1 H2 H3.
Tactic Notation "spec" hyp(H1) hyp(H2) hyp(H3) hyp(H4)
 := specializes H1 H2 H3 H4.
Tactic Notation "spec" hyp(H1) hyp(H2) hyp(H3) hyp(H4)
 := specializes H1 H2 H3 H4.
Tactic Notation "spec" hyp(H1) hyp(H2) hyp(H3) hyp(H4) hyp(H5)
 := specializes H1 H2 H3 H4 H5.
Tactic Notation "spec" hyp(H1) hyp(H2) hyp(H3) hyp(H4) hyp(H5) hyp(H6)
 := specializes H1 H2 H3 H4 H5 H6.

Tactic Notation "break" constr(E) :=
 let X := fresh "X" in remember (E) as X; destruct X.

Tactic Notation "breaka" constr(E) :=
 let X := fresh "X" in remember (E) as X; destruct X; auto.


(********************************************************************)
(* Rip apart compound hypothesis and goals.
   Then try auto to eliminate the easy stuff. *)
Ltac rip
 := try 
     (repeat
       (match goal with
                              |- forall _, _ => intros;     rip
        |                        |- _ /\ _   => split;      rip
        | [H: _ /\ _             |- _ ]      => inverts H;  rip
        | [H1: ?a -> ?b, H2: ?a  |-  _]      => spec H1 H2; rip
        end)); try auto.


(********************************************************************)
(* Apply rewrites from the hypotheses *)
Ltac rewritess
 := simpl; 
    match goal with
    | [H: eq _ _               |- _ ] => simpl; rewrite H; auto
    | [H: forall _,     eq _ _ |- _ ] => simpl; rewrite H; auto
    | [H: forall _ _,   eq _ _ |- _ ] => simpl; rewrite H; auto
    | [H: forall _ _ _, eq _ _ |- _ ] => simpl; rewrite H; auto
    end.

Ltac rs := rewritess.
Ltac rr := autorewrite with global in *.


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
   Can be expensive when inverting many of the hypothesis produce
   more premises.
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


(* Nope solves the goal completely or does nothing *)
Ltac nope 
 := first [ rip; solve [repeat nope1] 
          | idtac ].


(********************************************************************)
(* Burn mega-tactic for semantics proofs.

   * This gets lots of the common cases, where Coq really should 
     have tried a bit harder. For example, trying f_equal before eauto.

   * It also contains cases for patterns that often arise when using
     the deBruijn representation. For example, trying false before
     omega to show that a case concerning index manipulation
     cannot happen.

   * Failing is sometimes slow due to the (firstorder burn0) tactic
     in burn1.
 *)

(* Primitive tactics that fail quickly. *)
Ltac burn0
 := first
    [ assumption       (* Goal is one of the assumptions. *)
    | reflexivity      (* Goal has form e = e. *)
    | solve [eauto]    (* Resolution based proving *)
    | omega            (* Solves inequalities with Presburger arithmetic.  *)
    | false; omega ].  (* Solves inequalities with Presburger arithmetic.  *)

(* Try the firstorder solver.
   This can be slow if it fails. *)
Ltac burn1
 := first
    [ burn0 
    | solve [firstorder burn0] ].

(* Try to factor the goal in some other way before applying
   one of the primitive tactics. *)
Ltac burn2
 := first
    [ burn1
    | f_equal; burn1 ].

(* Apply normalising rewrite rules in various combinations.
   We need to try different combinations. Simplifying can cause rewrite
   rules not to fire, or to not have the form required for a auto rule,
   so we want to try burn2 before and after rewrites. *)
Ltac burn3
 := first [ burn2
          | try rr; 
            first [ burn2
                  | try rs;
                    first [ burn2
                          | simpl; burn2 ]]].

(* Apply common factorings that should always make goals easier
   to solve. 

   Try to use injectivity between two constructor applications.
   These are common when we lookup values from the environment, 
    eg with  get ix tyenv = Some t1. *)
Ltac burn4
 := rip; match goal with 
      [ H1 : _ = ?C ?y1
      , H2 : _ = ?C ?y2 |- _]
      => assert (y1 = y2); rewrite H1 in H2; inverts H2; burn3

    | [ H1 : ?C ?y1 = _
      , H2 : ?C ?y2 = _ |- _]
      => assert (y1 = y2); rewrite H1 in H2; inverts H2; burn3

    | _ => burn3
 end.

(* Top-level megatactic.
   Try some simple, fast things first, then try everything. *)
Ltac burn 
 := first [burn0 | burn4].


(********************************************************************)
(* Assert a statement and prove it via burn.
   This leads to more structured proving than using plain 'assert', 
   because a 'have' form must always complete the goal. *)

Tactic Notation "have" constr(E) :=
 let H := fresh 
 in assert E as H by burn.

Tactic Notation "have" constr(E) "as" ident(H) :=
 assert E as H by burn.

Tactic Notation "have" constr(E) "by" tactic(T) :=
 let H := fresh 
 in assert E as H by T.

Tactic Notation "have" constr(E) "as" ident(H) "by" tactic(T) :=
 assert E as H by T.


(********************************************************************)
(* Rewrite using burn.
   Just state the equality to use. *)
Tactic Notation "rrwrite" constr(xx)
 := let H := fresh 
    in assert xx as H by burn; rewrite H; clear H.

Tactic Notation "rrwrite" constr(xx) "in" hyp(H)
 := let H2 := fresh
    in  assert xx as H2 by burn; rewrite H2 in H; clear H2.

Tactic Notation "rw" constr(xx)             := rrwrite xx.
Tactic Notation "rw" constr(xx) "in" hyp(H) := rrwrite xx in H.


(********************************************************************)
(* Tactics specific to particular libraries *)

(* Tactics for working with forall. *)
(* Normalise foralls to In form. *)
Ltac nforall := 
 repeat
  (match goal with 
   | [ H: Forall _ _ |- _ ] => rewrite Forall_forall in H
   | [ H: _ |- Forall _ _ ] => rewrite Forall_forall
   end).


(* Breaking up nat_compare
   Find the first (nat_compare ?E1 ?E2) and destruct it into the
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


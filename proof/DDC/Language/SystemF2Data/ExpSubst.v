
Require Export DDC.Language.SystemF2Data.ExpBase.
Require Import DDC.Language.SystemF2Data.ExpLift.

(* Substitution of Types in Exps *)
Fixpoint substTX (d: nat) (u: ty) (xx: exp) : exp :=
  match xx with
  | XVar _     => xx

  |  XLAM x     
  => XLAM (substTX (S d) (liftTT 1 0 u) x)

  |  XAPP x t
  => XAPP (substTX d u x)  (substTT d u t)

  |  XLam t x
  => XLam (substTT d u t)  (substTX d u x)

  |  XApp x1 x2
  => XApp (substTX d u x1) (substTX d u x2)

  |  XCon dc ts xs 
  => XCon dc (map (substTT d u) ts) (map (substTX d u) xs)

  |  XCase xx alts
  => XCase (substTX d u xx) (map (substTA d u) alts)
 end

with substTA (d: nat) (u: ty) (aa: alt) : alt :=
  match aa with
  |  AAlt dc xx
  => AAlt dc (substTX d u xx)
  end.


(* Substitution of Exps in Exps *)
Fixpoint substXX (d:  nat) (u: exp) (xx: exp) : exp :=
 match xx with
    | XVar ix 
    => match nat_compare ix d with
       (* Index matches the one we are substituting for. *)
       | Eq  => u
       
       (* Index was free in the original expression.
          As we've removed the outermost binder, also decrease this
          index by one. *)
       | Gt  => XVar (ix - 1)

       (* Index was bound in the original expression. *)
       | Lt  => XVar ix
       end

    |  XLAM x1
    => XLAM (substXX d u x1)

    |  XAPP x1 t2
    => XAPP (substXX d u x1) t2

    (* Increase the depth as we move across a lambda.
       Also lift free references in the exp being substituted
       across the lambda as we enter it. *)
    |  XLam t1 x2
    => XLam t1 (substXX (S d) (liftXX 1 0 u) x2)

    (* Applications *)
    |  XApp x1 x2 
    => XApp (substXX d u x1) (substXX d u x2)

    |  XCon dc ts xs
    => XCon dc ts (map (substXX d u) xs)

    |  XCase x alts
    => XCase (substXX d u x) (map (substXA d u) alts)
    end

with substXA (d: nat) (u: exp) (aa: alt) 
 := match aa with 
    |  AAlt (DataCon tag arity) x 
    => AAlt (DataCon tag arity)
         (substXX (d + arity) 
                  (liftXX arity 0 u) x)
    end. 


(* Substitute several expressions.

   Note that in the definition, each time we substitute an 
   exp (u), we need to lift it by the number of exps remaining
   in the list (us). This is because we're placing the substitued
   exp under the binders corresponding to the remaining ones.

   The added lifting is then gradually "undone" each time we
   substitue one of the remaining expressions. This happens due
   to the free variable/Gt case in the definition of substX.

   Example:
               (A->B), A |- 0 :: A
               (A->B), A |- 1 :: (A->B)
    (A->B), A, A, (A->B) |- (0 1) [1 0] :: B
   
    Substitute first exp in list.
            (A->B), A, A |- (2 0) [0] :: B

    We get '2' by adding the length of the remaining substitution
    (1) to the index substituted (1). The argument of the function 
    is changed from 1 to 0 by the free variable case of substX.

    Substitute remaining exp in list.
               (A->B), A |- (1 0) :: B

    Here, 0 is subst for 0, and 2 changes to 1 due as it's a free
    variable.
*)
Fixpoint substXXs (d: nat) (us: list exp) (xx: exp) :=
 match us with
 | nil      => xx
 | u :: us' => substXXs d us' 
                 (substXX d (liftXX (List.length us') 0 u)
                            xx)
 end.


(* The data constructor of an alternative is unchanged
   by exp substitution. *)
Lemma dcOfAlt_substXA
 : forall d u a
 , dcOfAlt (substXA d u a) = dcOfAlt a.
Proof.
 intros. destruct a. destruct d0. auto.
Qed.
Hint Rewrite dcOfAlt_substXA : global.


(* The data constructor of an alternative is unchanged
   by type substitution. *)
Lemma dcOfAlt_substTA
 : forall d u a
 , dcOfAlt (substTA d u a) = dcOfAlt a.
Proof.
 intros. destruct a. destruct d0. auto.
Qed.
Hint Rewrite dcOfAlt_substTA : global.


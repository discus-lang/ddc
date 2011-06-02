
Require Export ExpBase.
Require Import ExpLift.


(** Substitution ****************************************************)
(* Substitute for the outermost binder in an expression. *)
Fixpoint
 substX (d:  nat) (* current binding depth in expression *)
        (u:  exp) (* new expression to substitute *)
        (xx: exp) (* expression to substitute into *)
        : exp 
 := match xx with
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

    (* Increase the depth as we move across a lambda.
       Also lift free references in the exp being substituted
       across the lambda as we enter it. *)
    |  XLam t1 x2
    => XLam t1 (substX (S d) (liftX 1 0 u) x2)

    (* Applications *)
    |  XApp x1 x2 
    => XApp (substX d u x1) (substX d u x2)

    |  XCon dc xs
    => XCon dc (List.map (substX d u) xs)

    |  XCase x alts
    => XCase (substX d u x) (List.map (substA d u) alts)
    end

with substA (d: nat) (u: exp) (aa: alt) 
 := match aa with 
    |  AAlt dc ts x 
    => AAlt dc ts (substX (d + List.length ts) 
                          (liftX (List.length ts) 0 u)
                           x)
    end. 

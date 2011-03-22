
Require Export Name.
Require Export Ty.
Require Export Cases.

(* expressions *******************************************)
Inductive exp : Type :=
  | XVar  : name -> exp
  | XLam  : name -> ty  -> exp -> exp
  | XApp  : exp  -> exp -> exp.


Tactic Notation "exp_cases" tactic(first) ident(c) :=
  first;
  [ Case_aux c "XVar" 
  | Case_aux c "XLam"
  | Case_aux c "XApp" ]. 


(* values **************************************************)
Inductive VALUE : exp -> Prop :=
 | VALUE_XLam  : forall x T t, VALUE (XLam  x T t).

Hint Constructors VALUE.


(* substitution *******************************************)
(* TODO: only works for s closed *)

Fixpoint subst (n : name) (s : exp) (t : exp) : exp :=
  match t with
    |  XVar n'      
    => if beq_name n n' then s else t

    |  XLam n' T x1
    => XLam n' T 
         (if beq_name n n'
              then x1
              else subst n s x1)

    |  XApp x1 x2 
    => XApp (subst n s x1) (subst n s x2)
  end.


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


(* free variables *****************************************)
Inductive freeX : name -> exp -> Prop :=
 | FreeX_var
   : forall n, freeX n (XVar n)

 | FreeX_lam
   :  forall x y T11 t12
   ,  y <> x
   -> freeX x t12
   -> freeX x (XLam y T11 t12)

 | FreeX_app1
   :  forall x t1 t2
   ,  freeX x t1 -> freeX x (XApp t1 t2)

 | FreeX_app2 
   :  forall x t1 t2
   ,  freeX x t2 -> freeX x (XApp t1 t2).

Tactic Notation "freeX_cases" tactic(first) ident(c) :=
  first;
  [ Case_aux c "FreeX_var"
  | Case_aux c "FreeX_lam"
  | Case_aux c "FreeX_app1"
  | Case_aux c "FreeX_app2"].

Hint Constructors freeX.

Definition closed (t:exp) 
 := forall x, ~(freeX x t).



(* freshness **********************************************)
Inductive freshX : name -> exp -> Prop :=
 | FreshX_var
   :  forall n1 n2
   ,  n1 <> n2 
   -> freshX n1 (XVar n2)

 | FreshX_lam
   :  forall n1 n2 T t11
   ,  n1 <> n2 
   -> freshX n1 t11 
   -> freshX n1 (XLam n2 T t11)

 | FreshX_app
   :  forall n1 t11 t12
   ,  freshX n1 t11
   -> freshX n1 t12
   -> freshX n1 (XApp t11 t12).


(* Substitution *******************************************)
Fixpoint subst (n : name) (s : exp) (t : exp) : exp :=
  match t with
    |  XVar n'      
    => if beq_name n n' then s else t

    |  XLam n' T x1
    => if beq_name n n'
        then XLam n' T x1
        else XLam n' T (subst n s x1)  

    |  XApp x1 x2 
    => XApp (subst n s x1) (subst n s x2)
  end.

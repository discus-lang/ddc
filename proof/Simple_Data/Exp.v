
Require Export Base.
Require Export Env.
Require Import Coq.Strings.String.


(* Types ************************************************************)
Inductive tycon : Type :=
 | TyConData   : string -> tycon.
Hint Constructors tycon.

Inductive ty : Type :=
 | TCon   : tycon -> ty
 | TFun   : ty    -> ty -> ty.
Hint Constructors ty.


(* Expressions ******************************************************
   We use deBruijn indices for binders.
 *)
Inductive datacon : Type :=
 | DataCon    : string -> datacon.
Hint Constructors datacon.

Inductive exp : Type :=
 (* Functions *************************)
 | XVar  : nat -> exp
 | XLam  : ty  -> exp -> exp
 | XApp  : exp -> exp -> exp

 (* Data Types ************************)
 | XCon  : datacon -> list exp -> exp
 | XCase : exp     -> list alt -> exp

with alt     : Type :=
 | AAlt  : datacon -> exp -> alt.

Hint Constructors exp.
Hint Constructors alt.


(* Definitions ******************************************************)
Inductive def  : Type :=
 (* Definition of a data type constructor *)
 | DefType 
   :  tycon        (* Name of data type constructor *)
   -> list datacon (* Data constructors that belong to this type *)
   -> def

 (* Definition of a data constructor *)
 | DefData 
   :  datacon      (* Name of data constructor *)
   -> list ty      (* Types of arguments *)
   -> tycon        (* tycon of constructed data *)
   -> def.
Hint Constructors def.


(* Type Environments ************************************************)
Definition tyenv := env ty.


(* Weak Head Normal Forms cannot be reduced further by 
   call-by-value evaluation.
 *)
Inductive whnfX : exp -> Prop :=
 | Whnf_XVar 
   : forall i
   , whnfX (XVar i)

 | Whnf_XLam
   : forall t1 x2
   , whnfX (XLam t1 x2)

 | Whnf_XCon
   :  forall dc xs
   ,  (forall i x, getl xs i = Some x -> whnfX x)
   -> whnfX (XCon dc xs).
Hint Constructors whnfX.


Fixpoint wfX (te: tyenv) (xx: exp) : Prop :=
 match xx with  
 | XVar  i       => exists t, Env.get te i = Some t
 | XLam  t x     => wfX (te :> t) x
 | XApp  x1 x2   => wfX te x1 /\ wfX te x2

 | XCon  dc xs   
 => (fix wfXs (xs': list exp) : Prop := 
     match xs' with 
     | nil       => True
     | x :: xs'' => wfX te x /\ wfXs xs''
     end) xs

 | XCase x alts 
 => (fix wfAs (as': list alt) : Prop :=
     match as' with 
     | nil        => True
     | a :: as''  => wfA te a /\ wfAs as''
     end) alts
 end

with    wfA (te: tyenv) (aa: alt) : Prop :=
 match aa with
 | AAlt dc x     => wfX te x
 end.


(* Closed expressions are well formed under an empty environment. *)
Definition closedX (xx: exp) : Prop
 := wfX Empty xx.
Hint Unfold closedX.


(* Values are closed expressions that cannot be reduced further. *)
Inductive value : exp -> Prop :=
 | Value 
   :  forall xx
   ,  whnfX xx -> closedX xx
   -> value xx.
Hint Constructors value.


(* Lifting **********************************************************)
(* When we push new elements on the environment stack of an
   expression, we need to lift free indices in the expression 
   across the new elements.

   For example given: 
             t1, t0 |- 0 1 (\. 0 1 2) :: t3

   Pushing two more elements gives:
     t1, t0, ta, tb |- 2 3 (\. 0 3 4) :: t3
 *)
Fixpoint 
 liftX  (d:  nat) (* current binding depth in expression *)
        (xx: exp) (* expression to lift *)
        : exp
 := match xx with 
    |  XVar ix    
    => if le_gt_dec d ix
        (* var was pointing into env, lift it across new elems *)
        then XVar (S ix)
        (* var was locally bound, leave it be *)
        else xx

    (* increase the depth as we move across a lambda *)
    |  XLam t1 x1
    => XLam t1 (liftX (S d) x1)

    |  XApp x1 x2
    => XApp   (liftX d x1) (liftX d x2)

    |  XCon dc xs
    => XCon dc (List.map (liftX d) xs)

    |  XCase x alts
    => XCase (liftX d x) (List.map (liftA d) alts)
    end

 with liftA (d: nat) (aa: alt) := 
  match aa with
  | AAlt dc x => AAlt dc (liftX d x)
  end.



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
    => XLam t1 (substX (S d) (liftX 0 u) x2)

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
    |  AAlt dc x 
    => AAlt dc (substX d u x)
    end. 





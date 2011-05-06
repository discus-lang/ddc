
Require Export Base.
Require Export Env.


(* Types ************************************************************)
Inductive tycon : Type :=
 | TyConBool  : tycon
 | TyConNat   : tycon.
Hint Constructors tycon.

Inductive ty  : Type :=
 | TCon       : tycon -> ty
 | TFun       : ty    -> ty -> ty.
Hint Constructors ty.

Definition tBool := TCon TyConBool.
Definition tNat  := TCon TyConNat.


(* Type Environments *)
Definition tyenv := env ty.


(* Expressions ******************************************************
   We use deBruijn indices for binders.
 *)
Inductive exp : Type :=
 (* Functions *)
 | XVar    : nat -> exp
 | XLam    : ty  -> exp -> exp
 | XApp    : exp -> exp -> exp

 (* Fixpoints *)
 | XFix    : ty  -> exp -> exp

 (* Naturals *)
 | XNat    : nat -> exp
 | XSucc   : exp -> exp
 | XPred   : exp -> exp

 (* Booleans *)
 | XTrue   : exp
 | XFalse  : exp
 | XIsZero : exp -> exp

 (* Branching *)
 | XIf     : exp -> exp -> exp -> exp.

Hint Constructors exp.


(* Weak Head Normal Forms cannot be reduced further by 
   call-by-value evaluation.
 *)
Inductive whnfX : exp -> Prop :=
 | Whnf_XVar 
   :  forall i
   ,  whnfX (XVar i)

 | Whnf_XLam
   :  forall t1 x2
   ,  whnfX (XLam t1 x2)

 | Whnf_XNat
   :  forall n
   ,  whnfX (XNat n)

 | Whnf_XTrue
   :  whnfX XTrue

 | Whnf_False
   :  whnfX XFalse.

Hint Constructors whnfX.


(* Well formed expressions are closed under the given environment. *)
Fixpoint wfX (te: tyenv) (xx: exp) : Prop :=
 match xx with  
 (* Functions *)
 | XVar i          => exists t, get te i = Some t
 | XLam t x        => wfX (te :> t) x
 | XApp x1 x2      => wfX te x1 /\ wfX te x2

 (* Fixpoints *)
 | XFix t x        => wfX (te :> t) x
 
 (* Naturals *)
 | XNat n          => True
 | XSucc x1        => wfX te x1
 | XPred x1        => wfX te x1

 (* Booleans *)
 | XTrue           => True
 | XFalse          => True
 | XIsZero x1      => wfX te x1

 (* Branching *)
 | XIf   x1 x2 x3  => wfX te x1 /\ wfX te x2 /\ wfX te x3
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
    |  XLam t1 x1   => XLam t1 (liftX (S d) x1)
    |  XApp x1 x2   => XApp    (liftX d x1) (liftX d x2)

    (* Fixpoints *)
    |  XFix t1 x1   => XFix t1 (liftX (S d) x1)

    (* Naturals *)
    |  XNat _       => xx
    |  XSucc x1     => XSucc   (liftX d x1)
    |  XPred x1     => XPred   (liftX d x1)

    (* Booleans *)
    |  XTrue        => xx
    |  XFalse       => xx
    |  XIsZero x1   => XIsZero (liftX d x1)

    (* Branching *)
    |  XIf x1 x2 x3 => XIf (liftX d x1) (liftX d x2) (liftX d x3)
    end.


(** Substitution ****************************************************)
(* Substitute for the outermost binder in an expression. *)
Fixpoint
 substX (d:  nat) (* current binding depth in expression *)
        (u:  exp) (* new expression to substitute *)
        (xx: exp) (* expression to substitute into *)
        : exp 
 := match xx with
    (* Functions **********************)
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
    |  XLam t1 x2   => XLam t1 (substX (S d) (liftX 0 u) x2)

    |  XApp x1 x2   => XApp (substX d u x1) (substX d u x2)

    (* Fixpoints **********************)
    |  XFix t1 x2   => XFix t1 (substX (S d) (liftX 0 u) x2)

    (* Naturals ***********************)
    |  XNat _       => xx
    |  XSucc x1     => XSucc (substX d u x1)
    |  XPred x1     => XPred (substX d u x1)

    (* Booleans **********************)
    |  XTrue        => xx
    |  XFalse       => xx
    |  XIsZero x1   => XIsZero (substX d u x1)

    (* Branching *********************)
    |  XIf x1 x2 x3 
    => XIf (substX d u x1) (substX d u x2) (substX d u x3) 
 end. 





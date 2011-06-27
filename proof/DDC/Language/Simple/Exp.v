Require Export DDC.Base.


(********************************************************************)
(* Types *)
Inductive ty  : Type :=
 | TCon  : nat -> ty           (* data type constructor *)
 | TFun  : ty  -> ty -> ty.    (* function type constructor *)
Hint Constructors ty.

(* Type Environments *)
Definition tyenv := list ty.


(********************************************************************)
(* Value Expressions *)
Inductive exp : Type :=
 | XVar  : nat -> exp          (* deBruijn index *)
 | XLam  : ty  -> exp -> exp   (* function abstraction *)
 | XApp  : exp -> exp -> exp.  (* function application *)
Hint Constructors exp.


(* Weak normal forms.
   Expressions in weak normal form cannot be reduced further by
   call-by-value evaluation.

   Note that c.b.v does not reduce under lambdas, which makes 
   the result of an evaluation a *weak* normal form (instead of
   a strong normal form).

   It's not *weak head* normal form because we use strict function
   application, so the function arguments are reduced to wnf before
   substituting. *)
Inductive wnfX : exp -> Prop :=
 | Wnf_XVar 
   : forall i
   , wnfX (XVar i)

 | Wnf_XLam
   : forall t1 x2
   , wnfX (XLam t1 x2).
Hint Constructors wnfX.


(* Well formed expressions are closed under the given environment. *)
Fixpoint wfX (te: tyenv) (xx: exp) : Prop :=
 match xx with  
 | XVar i     => exists t, get i te = Some t
 | XLam t x   => wfX (te :> t) x
 | XApp x1 x2 => wfX te x1 /\ wfX te x2
 end.


(* Closed expressions are well formed under an empty environment. *)
Definition closedX (xx: exp) : Prop
 := wfX nil xx.
Hint Unfold closedX.


(* Values are closed expressions that cannot be reduced further. *)
Inductive value : exp -> Prop :=
 | Value 
   :  forall xx
   ,  wnfX xx -> closedX xx
   -> value xx.
Hint Constructors value.


(********************************************************************)
(* Lifting of references into the environment.
   When we push new elements on the environment stack, we need
   to lift referenes to existing elements across the new ones. 

   For example given: 
             t1, t0 |- 0 1 (\. 0 1 2) :: t3

   Pushing two more elements gives:
     t1, t0, ta, tb |- 2 3 (\. 0 3 4) :: t3
 *)
Fixpoint 
 liftX  (d:  nat) (* current binding depth in expression *)
        (xx: exp) (* expression containing referenes to lift *)
        : exp
 := match xx with 
    |  XVar ix    
    => if le_gt_dec d ix
        (* Index is referencing the env, so lift it across the new elem *)
        then XVar (S ix)

        (* Index is locally bound in the expression itself, and 
           not the environment, so we don't need to change it. *)
        else xx

    (* Increase the current depth as we move across a lambda, 
       if we find locally bound indices that reference this lambda
       then we don't need to lift them. *)
    |  XLam t1 x1
    => XLam t1 (liftX (S d) x1)

    |  XApp x1 x2
    => XApp   (liftX d x1) (liftX d x2)
    end.


(*******************************************************************)
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
       Any free references in the exp being substitued also need
       to be lifted across the lambda as we enter it. *)
    |  XLam t1 x2
    => XLam t1 (substX (S d) (liftX 0 u) x2)

    |  XApp x1 x2 
    => XApp (substX d u x1) (substX d u x2)
 end. 

Require Export DDC.Base.

(********************************************************************)
(* Types *)
Inductive ty  : Type :=
 | TCon  : nat -> ty               (* data type constructor *)
 | TFun  : ty  -> ty -> ty         (* function type constructor *)
 | TRef  : ty  -> ty.              (* reference type constructor *)
Hint Constructors ty.

(* Baked in types. *)
Definition tUnit  := TCon 0.

(* Type Environment. *)
Definition tyenv := list ty.

(* Store typing. *)
Definition stenv := list ty.


(********************************************************************)
(* Value Expressions *)
Inductive exp : Type :=
 (* Base calculus. *)
 | XCon      : nat  -> exp         (* data constructor *)
 | XVar      : nat  -> exp         (* deBruijn index *)
 | XLam      : ty   -> exp -> exp  (* function abstraction *)
 | XApp      : exp  -> exp -> exp  (* function application *)

 (* Primitive operators.
    We want to define these in fully applied form because so we don't
    need to worry about partial application in the typing rules. We
    can't use XApp directly becuase the primitives need to be
    polymorphic but we only have a monomorphic base calculus. *)
 | XNewRef   : exp -> exp          (* allocate a new reference *)
 | XReadRef  : exp -> exp          (* read a references *)
 | XWriteRef : exp -> exp -> exp   (* write to a reference *)

 (* Expressions created during reduction, 
    and not usually part of the initial program. *)
 | XLoc      : nat  -> exp.         (* store location *)
Hint Constructors exp.


(* Baked in expressions *)
Definition xUnit := XCon 0.
Hint Unfold xUnit.


(* Weak normal forms.
   Expressions in weak normal form cannot be reduced further by
   call-by-value evaluation. *)
Inductive wnfX : exp -> Prop :=
 | Wnf_XCon 
   : forall n
   , wnfX (XCon n)

 | Wnf_XVar 
   : forall i
   , wnfX (XVar i) 

 | Wnf_XLam
   : forall t1 x2
   , wnfX (XLam t1 x2)

 | Wnf_XLoc
   : forall l
   , wnfX (XLoc l).
Hint Constructors wnfX.

Check Wnf_XVar.

(* Well formed expressions are closed under the given environment. *)
Fixpoint wfX (te: tyenv) (xx: exp) : Prop :=
 match xx with  
 | XCon _          => True
 | XVar i          => exists t, get i te = Some t
 | XLam t x        => wfX (te :> t) x
 | XApp x1 x2      => wfX te x1 /\ wfX te x2
 | XNewRef x       => wfX te x
 | XReadRef x      => wfX te x
 | XWriteRef x1 x2 => wfX te x1 /\ wfX te x2
 | XLoc _          => True
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

Lemma value_wnfX 
 : forall xx, value xx -> wnfX xx.
 Proof. intros. inverts H. auto. Qed.
Hint Resolve value_wnfX.

Lemma value_closedX 
 : forall xx, value xx -> closedX xx.
 Proof. intros. inverts H. auto. Qed.
Hint Resolve value_closedX.


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
    |  XCon i
    => XCon i

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

    |  XNewRef x1 
    => XNewRef (liftX d x1)

    |  XReadRef x1
    => XReadRef (liftX d x1)
 
    |  XWriteRef x1 x2
    => XWriteRef (liftX d x1) (liftX d x2)

    |  XLoc  l  
    => XLoc  l
    end.


(*******************************************************************)
(* Substitute for the outermost binder in an expression. *)
Fixpoint
 substX (d:  nat) (* current binding depth in expression *)
        (u:  exp) (* new expression to substitute *)
        (xx: exp) (* expression to substitute into *)
        : exp 
 := match xx with
    |  XCon i
    => XCon i

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

    |  XNewRef x1
    => XNewRef (substX d u x1)

    |  XReadRef x1
    => XReadRef (substX d u x1)

    |  XWriteRef x1 x2
    => XWriteRef (substX d u x1) (substX d u x2)
    
    |  XLoc l
    => XLoc l
 end. 


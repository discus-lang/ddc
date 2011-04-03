
Require Export Name.
Require Export Ty.


(* Expressions ******************************************************)
Inductive exp : Type :=
  | XVar  : name -> exp

  | XLam  : name -> ty   -> exp -> exp
  | XApp  : exp  -> exp  -> exp

  | XLAM  : name -> exp  -> exp
  | XAPP  : exp  -> ty   -> exp.

Hint Constructors exp.


(* Free variables ***************************************************
   This is defined on names, so we can ask for just the names in a
   particular namespace.
 *)
Inductive freeX : name -> exp -> Prop :=
 | FreeX_var
   : forall n
   , freeX n (XVar n)

 | FreeX_lam
   :  forall x y T11 t12
   ,  x <> y
   -> freeX x t12
   -> freeX x (XLam y T11 t12)

 | FreeX_app1
   :  forall x t1 t2
   ,  freeX x t1 -> freeX x (XApp t1 t2)

 | FreeX_app2
   :  forall x t1 t2
   ,  freeX x t2 -> freeX x (XApp t1 t2)

 | FreeX_LAM
   :  forall x y t12 
   ,  x <> y
   -> freeX x t12
   -> freeX x (XLAM y t12)

 | FreeX_APP1
   :  forall x t1 T2
   ,  freeX x t1
   -> freeX x (XAPP t1 T2)

 | FreeX_APP2
   :  forall x t1 T2
   ,  freeT x T2
   -> freeX x (XAPP t1 T2).

Hint Constructors freeX.
Hint Resolve FreeX_var.


(* Variable Binding *************************************************
   This is used to check that a term does not bind a particular
   name, to help ward against variable capture issues.
*)
Inductive bindsX : name -> exp -> Prop :=
 | BindsX_lam_bound
   :  forall x T11 t12
   ,  bindsX x (XLam x T11 t12)

 | BindsX_lam
   :  forall v y T11 t12
   ,  bindsT v T11 \/ bindsX v t12
   -> bindsX v (XLam y T11 t12)

 | BindsX_app
   :  forall v t11 t12
   ,  bindsX v t11 \/ bindsX v t12
   -> bindsX v (XApp t11 t12)

 | BindsX_LAM_bound
   :  forall a t1
   ,  bindsX a (XLAM a t1)

 | BindsX_LAM
   :  forall v a t11
   ,  bindsX v t11
   -> bindsX v (XLAM a t11)

 | BindsX_APP
   :  forall v t11 T12
   ,  bindsX v t11 \/ bindsT v T12
   -> bindsX v (XAPP t11 T12).

Hint Constructors bindsX.


(* Well Formedness **************************************************
   This checks the binding structure of the expression, 
     ensure all names are in the correct space
     check for shadowing: (\x. \x. t)
     check for same var being both bound and free.
       this is bad: (\x. x) x   this is ok: (\x. x) (\x. x)
*)
(* Shift namespace premises to this judgement.
   Doesn't work (\y. \x. y) (\x. x).. results in shadowing.
   Maybe rewrite binders on every substitution.
*)
          




(* Substitution of Types in Exps ************************************)
Fixpoint substTX (x : name) (S : ty)  (t : exp) : exp := 
  match t with
    |  XVar _ => t

    |  XLam x' T1 t2
    => XLam x' (substTT x S T1) (substTX x S t2)

    |  XApp t1 t2
    => XApp (substTX x S t1) (substTX x S t2)

    |  XLAM x' t1
    => if beq_name x x'
        then XLAM x' t1
        else XLAM x' (substTX x S t1)

    |  XAPP t1 T2
    => XAPP (substTX x S t1) (substTT x S T2)
  end.


(* Substitution of Exps in Exps *************************************)
Fixpoint substXX (x : name) (s : exp) (t : exp) : exp :=
  match t with
    |  XVar x'      
    => if beq_name x x' then s else t

    |  XLam x' T t1
    => if beq_name x x'
        then XLam x' T t1
        else XLam x' T (substXX x s t1)  

    |  XApp t1 t2 
    => XApp (substXX x s t1) (substXX x s t2)

    |  XLAM x' t1
    => XLAM x' (substXX x s t1)

    |  XAPP t1 T2
    => XAPP (substXX x s t1) T2
  end.


(* Noncapturing *****************************************************
   If a name is free is a lambda expression, then we know that
   it's not the name being bound by it. 
 *)
Lemma nocapture_lam
 : forall x y T t
 , freeX x (XLam y T t) -> x <> y.
Proof.
 intros. inversion H. subst. auto.
Qed.


Lemma nocapture_LAM
 : forall a b t
 , freeX a (XLAM b t) -> a <> b.
Proof.
 intros. inversion H. subst. auto.
Qed.


(* Closedness *******************************************************)
Definition closedX (t:exp) 
 := forall x, ~(freeX x t).

Hint Unfold closedX.

(* TODO: Check are we really using these lemmas? *)

Theorem closedX_var_not
 : forall n
 , ~(closedX (XVar n)).
Proof.
 intros. intro.
 unfold closedX in H. specialize H with n. auto. 
Qed.


Theorem closedX_lam
 : forall x T t 
 , closedX t -> closedX (XLam x T t).
Proof. 
 intros. unfold closedX. intros. intro.
 inversions H0.
 unfold closedX in H. apply H in H6. auto.
Qed.


Theorem closed_app
 : forall t1 t2
 , closedX t1 -> closedX t2 -> closedX (XApp t1 t2).
Proof. 
 intros.
 unfold closedX. intros. intro.
 unfold closedX in H.  specialize H  with x.
 unfold closedX in H0. specialize H0 with x.
 inversion H1; subst; tauto.
Qed.


(* Values ***********************************************************
   We require values to be closed, to make it easy to pass this 
   closedness information out of the STEP relation.
   In the proof of progress, if STEP produces a value, then it's closed.
 *)
Inductive value : exp -> Prop :=
 | Value_lam  
    : forall x T t
    , closedX (XLam x T t) -> value (XLam  x T t)

 | Value_LAM
    : forall x t
    , closedX (XLAM x t)   -> value (XLAM x t).

Hint Constructors value.


Lemma values_are_closed
 : forall t
 , value t -> closedX t.
Proof.
 intros. inversion H.
 Case "XLam". auto.
 Case "XLAM". auto.
Qed.


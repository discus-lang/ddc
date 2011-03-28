
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


(* Freshness ********************************************************
   This is used to check that a term does not bind a particular
   name, to help ward against variable capture issues.
 *)
Inductive freshX : name -> exp -> Prop :=
 | FreshX_var
   :  forall x by
   ,  freshX x (XVar by)

 | FreshX_lam
   :  forall x y T t11
   ,  x <> y
   -> freshX x t11 
   -> freshX x (XLam y T t11)

 | FreshX_app
   :  forall x t11 t12
   ,  freshX x t11
   -> freshX x t12
   -> freshX x (XApp t11 t12).

Hint Constructors freshX.


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
Definition closed (t:exp) 
 := forall x, ~(freeX x t).


Theorem closed_var_not
 : forall n
 , ~(closed (XVar n)).
Proof.
 intro. unfold not. intro.
 unfold closed in H. specialize H with n. auto. 
Qed.


Theorem closed_lam
 : forall x T t 
 , closed t -> closed (XLam x T t).
Proof. 
 intros. unfold closed. intros. unfold not. intro.
 inversion H0. subst.
 unfold closed in H. apply H in H6. assumption.
Qed.


Theorem closed_app
 : forall t1 t2
 , closed t1 -> closed t2 -> closed (XApp t1 t2).
Proof. 
 intros.
 unfold closed. intros. unfold not. intro.
 unfold closed in H.  specialize H  with x.
 unfold closed in H0. specialize H0 with x.
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
    , closed (XLam x T t) -> value (XLam  x T t)

 | Value_LAM
    : forall x t
    , closed (XLAM x t)   -> value (XLAM x t).

Hint Constructors value.


Lemma values_are_closed
 : forall t
 , value t -> closed t.
Proof.
 intros. inversion H.
 Case "XLam". auto.
 Case "XLAM". auto.
Qed.


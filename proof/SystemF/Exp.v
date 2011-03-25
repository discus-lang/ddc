
Require Export Name.
Require Export Ty.

(* expressions *******************************************)
Inductive exp : Type :=
  | XVar  : name -> exp

  | XLam  : name -> ty  -> exp -> exp
  | XApp  : exp  -> exp -> exp

  | XLAM  : name -> exp  -> exp
  | XAPP  : exp  -> ty   -> exp.


(* Substitution of Types in Exps *************************)
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



(* Substitution of Exps in Exps **************************)
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
   ,  freeX x t2 -> freeX x (XApp t1 t2)

 | FreeX_LAM
   :  forall x y t12 
   ,  y <> x
   -> freeX x t12
   -> freeX x (XLAM y t12)

 | FreeX_APP1
   :  forall x t1 T2
   ,  freeT x T2 -> freeX x (XAPP t1 T2)

 | FreeX_APP2
   :  forall x t1 T2
   ,  freeX x t1 -> freeX x (XAPP t1 T2).

Hint Constructors freeX.
Hint Resolve FreeX_var.


(* If a variable is free is a lambda expression, then we know 
   it's not the variable being bound. *)
Lemma nocapture_lam
 : forall x y T t
 , freeX x (XLam y T t) -> x <> y.
Proof.
 intros. inversion H. subst. auto.
Qed.


(* closed *************************************************)
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
 inversion H1. auto. auto.
Qed.


(* values **************************************************)
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


(* freshness **********************************************
   This is used to check that a term does not bind a particular
   variable, to help ward against variable capture issues  *)

Inductive freshX : name -> exp -> Prop :=
 | FreshX_var
   :  forall n1 n2
   ,  freshX n1 (XVar n2)

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

Hint Constructors freshX.




Require Export Name.
Require Export Ty.

(* expressions *******************************************)
Inductive exp : Type :=
  | XVar  : name -> exp
  | XLam  : name -> ty  -> exp -> exp
  | XApp  : exp  -> exp -> exp.


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
    , closed (XLam x T t) -> value (XLam  x T t).

Hint Constructors value.


Lemma values_are_closed
 : forall t
 , value t -> closed t.
Proof.
 intros. inversion H. auto.
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

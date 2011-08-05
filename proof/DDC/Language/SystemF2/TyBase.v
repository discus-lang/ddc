
Require Export DDC.Language.SystemF2.Ki.


(********************************************************************)
(* Type Constructors. *)
Inductive tycon : Type :=
 | TyConFun  : tycon
 | TyConData : nat   -> ki -> tycon.
Hint Constructors tycon.

Definition isTyConFun  (tc: tycon) : Prop :=
 match tc with
 | TyConFun      => True
 | TyConData _ _ => False
 end.
Hint Unfold isTyConFun.

Definition isTyConData (tc: tycon) : Prop :=
 match tc with
 | TyConFun      => False
 | TyConData _ _ => True
 end.
Hint Unfold isTyConData.


(********************************************************************)
(* Type Expressions. *)
Inductive ty  : Type :=
 | TCon      : tycon -> ty
 | TVar      : nat   -> ty
 | TForall   : ty    -> ty
 | TApp      : ty    -> ty -> ty.
Hint Constructors ty.


(* Baked in types. *)
Definition tFun (t1: ty) (t2: ty)
 := TApp (TApp (TCon TyConFun) t1) t2.
Hint Unfold tFun.


(* Get the type constructor of a type, if any *)
Fixpoint getCtorOfType (tt: ty) : option tycon :=
 match tt with
 | TCon tc   => Some tc
 | TApp t1 _ => getCtorOfType t1
 | _         => None
 end.


(* Construct a type application from a constructor type
   and a list of argument types. *)
Fixpoint makeTApps (t1: ty) (tt: list ty) : ty :=
 match tt with
 | nil     => t1
 | t :: ts => makeTApps (TApp t1 t) ts
 end.


Lemma makeTApps_snoc
 : forall t1 t2 t3 ts
 , makeTApps (TApp t1 t2) (snoc t3 ts) 
 = TApp (makeTApps t1 (cons t2 ts)) t3.
Proof.
 intros. gen t1 t2.
 induction ts; intros.
  simpl. auto.
  simpl. rewrite IHts. auto.
Qed.


(* Break apart a type application into the constructor type
   and a list of argument types. *)
Fixpoint takeTApps (tt: ty) : (ty * list ty) :=
 match tt with
 | TApp t1 t2 => match takeTApps t1 with
                 | (t11, t1s) => (t1, snoc t2 t1s )
                 end
 | _          => (tt, nil)
 end.

Fixpoint takeTCon (tt: ty) : ty :=
 match tt with 
 | TApp t1 t2 => takeTCon t1
 | _          => tt
 end.


Lemma takeTCon_makeTApps
 :  forall t1 ts
 ,  takeTCon (makeTApps t1 ts) = takeTCon t1.
Proof.
 intros. gen t1.
 induction ts; intros.
  simpl. auto.
  simpl.
  rewrite IHts. simpl. auto.
Qed.    


Lemma makeTApps_takeTCon
 : forall t1 t2 ts  
 ,  makeTApps t1 ts = t2
 -> takeTCon t1     = takeTCon t2.
Proof.
 intros. gen t1 t2.
 induction ts; intros.
  simpl in H. subst. auto.

  eapply IHts in H.
  simpl in H. auto.
Qed.








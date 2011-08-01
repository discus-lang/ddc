
Require Export DDC.Language.SystemF2.TyEnv.
Require Export DDC.Language.SystemF2.Ty.
Require Export DDC.Language.SystemF2.Ki.


(* Expressions *)
Inductive exp : Type :=
 | XVar  : nat -> exp
 | XLAM  : exp -> exp
 | XAPP  : exp -> ty  -> exp
 | XLam  : ty  -> exp -> exp
 | XApp  : exp -> exp -> exp.
Hint Constructors exp.


(* Weak normal forms cannot be reduced further by 
   call-by-value evaluation. *)
Inductive wnfX : exp -> Prop :=
 | Wnf_XVar 
   : forall i
   , wnfX (XVar i)

 | Wnf_XLAM
   : forall x1
   , wnfX (XLAM x1)

 | Wnf_XLam
   : forall t1 x2
   , wnfX (XLam t1 x2).
Hint Constructors wnfX.


(* A well formed expression is closed under the given environments *)
Fixpoint wfX (ke: kienv) (te: tyenv) (xx: exp) : Prop := 
 match xx with 
 | XVar i     => exists t, get i te = Some t
 | XLAM x     => wfX (ke :> KStar) (liftTE 0 te) x
 | XAPP x t   => wfX ke te x  /\ wfT ke t
 | XLam t x   => wfT ke t     /\ wfX ke (te :> t) x
 | XApp x1 x2 => wfX ke te x1 /\ wfX ke te x2
 end.
Hint Unfold wfX.


(* A closed expression is well formed under an empty environment. *)
Definition closedX (xx: exp) : Prop
 := wfX nil nil xx.
Hint Unfold closedX.


(* Values are closed expressions that cannot be reduced further *)
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
(* Lift type indices in expressions. *)
Fixpoint liftTX (d: nat) (xx: exp) : exp :=
  match xx with
  |  XVar _     => xx

  |  XLAM x     
  => XLAM (liftTX (S d) x)

  |  XAPP x t 
  => XAPP (liftTX d x)   (liftTT 1 d t)
 
  |  XLam t x   
  => XLam (liftTT 1 d t) (liftTX d x)

  |  XApp x1 x2
  => XApp (liftTX d x1)  (liftTX d x2)
 end.


(* Lift value indices in expressions. *)
Fixpoint liftXX (d: nat) (xx: exp) : exp :=
  match xx with
  |  XVar ix    
  => if le_gt_dec d ix
      then XVar (S ix)
      else xx

  |  XLAM x
  => XLAM (liftXX d x)

  |  XAPP x t
  => XAPP (liftXX d x) t
 
  |  XLam t x   
  => XLam t (liftXX (S d) x)

  |  XApp x1 x2
  => XApp (liftXX d x1) (liftXX d x2)
 end.


(********************************************************************)
(* Substitution of Types in Exps *)
Fixpoint substTX (d: nat) (u: ty) (xx: exp) : exp :=
  match xx with
  | XVar _     => xx

  |  XLAM x     
  => XLAM (substTX (S d) (liftTT 1 0 u) x)

  |  XAPP x t
  => XAPP (substTX d u x)  (substTT d u t)

  |  XLam t x
  => XLam (substTT d u t)  (substTX d u x)

  |  XApp x1 x2
  => XApp (substTX d u x1) (substTX d u x2)
 end.


(* Substitution of Exps in Exps *)
Fixpoint substXX (d: nat) (u: exp) (xx: exp) : exp :=
  match xx with
  | XVar ix    
  => match nat_compare ix d with
     | Eq => u
     | Gt => XVar (ix - 1)
     | _  => XVar  ix
     end

  |  XLAM x
  => XLAM (substXX d (liftTX 0 u) x)

  |  XAPP x t
  => XAPP (substXX d u x) t

  |  XLam t x
  => XLam t (substXX (S d) (liftXX 0 u) x)

  |  XApp x1 x2
  => XApp (substXX d u x1) (substXX d u x2)
  end.




Require Export Env.
Require Import Base.

(* Kinds ************************************************************)
Inductive ki : Type :=
 | KStar   : ki.

Definition kienv := env ki.
Hint Unfold kienv.


(** Types ***********************************************************)
Inductive ty  : Type :=
 | TCon    : nat -> ty
 | TVar    : nat -> ty
 | TForall : ty  -> ty
 | TFun    : ty  -> ty -> ty.
Hint Constructors ty.

Definition tyenv := env ty.
Hint Unfold tyenv.


(* Lift type indices that are at least a certain depth. *)
Fixpoint liftTT (d: nat) (tt: ty) : ty :=
  match tt with
  | TCon _     => tt

  |  TVar ix
  => if le_gt_dec d ix
      then TVar (S ix)
      else tt

  |  TForall t 
  => TForall (liftTT (S d) t)

  |  TFun t1 t2
  => TFun    (liftTT d t1) (liftTT d t2)
  end.
Hint Unfold liftTT.

(* Lift type indices in type environments. *)
Definition liftTE d te    := map (liftTT d) te.
Hint Unfold liftTE.

Ltac lift_cases 
 := match goal with 
     |  [ |- context [le_gt_dec ?n ?n'] ]
     => case (le_gt_dec n n')
    end.


(** Expressions *****************************************************)
Inductive exp : Type :=
 | XVar  : nat -> exp
 | XLAM  : exp -> exp
 | XAPP  : exp -> ty  -> exp
 | XLam  : ty  -> exp -> exp
 | XApp  : exp -> exp -> exp.
Hint Constructors exp.


(* Lift value indices in expressions.
   That are greater or equal to a given depth. *)
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



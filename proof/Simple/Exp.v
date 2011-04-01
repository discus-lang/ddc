
Require Export Env.


(** Types *************************************************)
Inductive ty  : Type :=
 | TCon  : nat -> ty
 | TFun  : ty  -> ty -> ty.

Hint Constructors ty.


(** Expressions *******************************************)
Inductive exp : Type :=
 | XVar  : nat -> exp
 | XLam  : ty  -> exp -> exp
 | XApp  : exp -> exp -> exp.

Hint Constructors exp.


(** Closedness ********************************************)
Inductive coversX : nat -> exp -> Prop :=
 | CoversX_var
   :  forall n i
   ,  (n >= i) 
   -> coversX n (XVar i)

 | CoversX_lam
   :  forall n T t
   ,  coversX (S n) t 
   -> coversX n (XLam T t)

 | CoversX_app
   :  forall n t1 t2
   ,  coversX n t1
   -> coversX n t2
   -> coversX n (XApp t1 t2).


Inductive closedX : exp -> Prop :=
 | ClosedX_lam
   :  forall T t
   ,  coversX O t
   -> closedX (XLam T t)

 | ClosedX_app
   :  forall t1 t2
   ,  closedX t1
   -> closedX t2
   -> closedX (XApp t1 t2).


Inductive value : exp -> Prop :=
 | Value_lam 
   : forall T t
   , closedX (XLam T t) -> value (XLam T t).

Hint Constructors value.



(** Type Judgements ***************************************)
Definition tyenv := env ty.

Fixpoint get (xx: env ty) (i: nat) {struct xx} : option ty :=
 match xx, i with
 | snoc _ T,  O    => some T
 | snoc xs _, S i' => get  xs i'
 | _, _            => none
 end.


Inductive TYPE : tyenv -> exp -> ty -> Prop :=
 | TYVar 
   :  forall tenv i T
   ,  get tenv i = some T
   -> TYPE tenv (XVar i) T

 | TYLam
   :  forall tenv t T1 T2
   ,  TYPE (tenv :> T1) t T2
   -> TYPE tenv (XLam T1 t) (TFun T1 T2)

 | TYApp
   :  forall tenv t1 t2 T1 T2
   ,  TYPE tenv t1 (TFun T1 T2)
   -> TYPE tenv t2 T1
   -> TYPE tenv (XApp t1 t2) T2.

Hint Constructors TYPE.

(** Application ******************************************)
Fixpoint applyX' (depth: nat) (tt: exp) (u: exp) : exp :=
 match tt with
 | XVar n     => if beq_nat n depth then u else tt
 | XLam T1 t2 => XLam T1 (applyX' (S depth) t2 u)
 | XApp t1 t2 => XApp (applyX' depth t1 u) (applyX' depth t2 u)
 end. 

Definition applyX := applyX' 0.
Hint Unfold applyX.

(** Evaluation *******************************************)
Inductive STEP : exp -> exp -> Prop :=
 |  EVLamApp
    : forall T11 t12 tv2
    ,  value tv2
    -> STEP (XApp   (XLam T11 t12) tv2)
            (applyX t12 tv2)

 |  EVApp1 
    :  forall t1 t1' t2
    ,  STEP t1 t1'
    -> STEP (XApp t1 t2) (XApp t1' t2)

 |  EVApp2 
    :  forall tv1 t2 t2'
    ,  value tv1
    -> STEP t2 t2'
    -> STEP (XApp tv1 t2) (XApp tv1 t2').

Hint Constructors STEP.





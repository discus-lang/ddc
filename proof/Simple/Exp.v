
Require Export Base.
Require Export Env.


(** Types *************************************************)
Inductive ty  : Type :=
 | TCon  : nat -> ty
 | TFun  : ty  -> ty -> ty.

Hint Constructors ty.


(** Type Environments *************************************)
Definition tyenv := env ty.


(** Expressions *******************************************)
Inductive exp : Type :=
 | XVar  : nat -> exp
 | XLam  : ty  -> exp -> exp
 | XApp  : exp -> exp -> exp.

Hint Constructors exp.


(** Closedness ********************************************)

(* Expression is closed under an enviornment of a given size *)
Inductive coversX : nat -> exp -> Prop :=
 | CoversX_var
   :  forall n i
   ,  (n > i) 
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
Hint Constructors coversX.

(* Expression is closed under the given environment. *)
Inductive closedUnderX : tyenv -> exp -> Prop :=
 | ClosedUnderX 
   : forall tenv x 
   , coversX (length tenv) x -> closedUnderX tenv x. 


(* Expression is closed under an empty environment, 
   it has no free locally bound varirables *)
Inductive closedX : exp -> Prop :=
 | ClosedX 
   : forall xx
   , coversX 0 xx -> closedX xx.


Inductive value : exp -> Prop :=
 | Value_lam 
   : forall T t
   , closedX (XLam T t) -> value (XLam T t).

Hint Constructors value.


(** Type Judgements ***************************************)
Inductive TYPE : tyenv -> exp -> ty -> Prop :=
 | TYVar 
   :   forall tenv i T
   ,   get tenv i = some T
   ->  TYPE tenv (XVar i) T  
       (* we want to know length of tenv i < length tenv
          makes it locally closed *)

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


(** Substitution ******************************************)
Fixpoint drop (n: nat) (tenv: tyenv) : tyenv :=
 match n, tenv with
  | _,     empty      => empty
  | O,     tenv :> T  => tenv
  | S n',  tenv :> T  => drop n' tenv :> T
  end.


Fixpoint liftX (n: nat) (depth: nat) (tt: exp) : exp :=
 match tt with 
 | XVar ix    => if bge_nat ix depth
                  then XVar (ix + n)
                  else tt

 | XLam T1 t1 => XLam T1 (liftX n (depth + 1) t1)

 | XApp t1 t2 => XApp (liftX n depth t1)
                      (liftX n depth t2)
 end.


Fixpoint subLocalX' (depth: nat) (u: exp) (tt: exp)  : exp :=
 match tt with
 | XVar ix    =>  match compare ix depth with
                  | EQ => liftX depth 0 u
                  | GT => XVar (ix - 1)
                  | _  => XVar ix
                  end

 | XLam T1 t2 => XLam T1 (subLocalX' (S depth) u t2)

 | XApp t1 t2 => XApp (subLocalX' depth u t1)
                      (subLocalX' depth u t2)
 end. 


Definition  subLocalX := subLocalX' 0.
Hint Unfold subLocalX.




(** Evaluation *******************************************)
Inductive STEP : exp -> exp -> Prop :=
 |  EVLamApp
    : forall T11 t12 tv2
    ,  value tv2
    -> STEP (XApp   (XLam T11 t12) tv2)
            (subLocalX tv2 t12)

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





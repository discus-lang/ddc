
Require Import SubstTypeValue.
Require Import SubstValueValue.
Require Import WellFormed.
Require Import Exp.


(* Single Step Evaluation *******************************************)
Inductive STEP : exp -> exp -> Prop := 
  (* value applications *)
  | ESLamApp
    :  forall t11 x12 v2
    ,  value v2
    -> STEP (XApp (XLam t11 x12) v2) (substXX 0 v2 x12)

  | ESApp1
    :  forall x1 x1' x2
    ,  STEP x1 x1'
    -> STEP (XApp x1 x2) (XApp x1' x2)
 
  | ESApp2
    :  forall v1 x2 x2'
    ,  value v1
    -> STEP x2 x2'
    -> STEP (XApp v1 x2) (XApp v1 x2')

  (* type applications *)
  | ESLAMAPP
    :  forall x12 t2      
    ,  STEP (XAPP (XLAM x12) t2) (substTX 0 t2 x12)

  | ESAPP1
    :  forall x1 x1' t2
    ,  STEP (XAPP x1 t2) (XAPP x1' t2).


        


Require Export Exp.

Inductive STEP : exp -> exp -> Prop := 
  | EVAppLam 
    :   forall x T t12 v2 
    ,   value v2 
    ->  STEP (XApp (XLam x T t12) v2)
             (substXX x v2 t12)

  | EVApp1 
    :   forall t1 t1' t2
    ,   STEP t1 t1'
    ->  STEP (XApp t1 t2) (XApp t1' t2)

  | EVApp2
    :   forall v1 t2 t2'
    ,   value v1
    ->  STEP t2 t2'
    ->  STEP (XApp v1 t2) (XApp v1 t2')

  | EVAPPLAM
    :   forall x t12 T2
    ,   closedT T2 
    ->  STEP (XAPP (XLAM x t12) T2)
             (substTX x T2 t12)

  | EVAPP1
    :   forall t1 T2 t1'
    ,   STEP t1 t1'
    ->  STEP (XAPP t1 T2) (XAPP t1' T2).


Hint Constructors STEP.

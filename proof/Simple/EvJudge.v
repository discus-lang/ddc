
Require Export Exp.
Require Export Substitute.


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

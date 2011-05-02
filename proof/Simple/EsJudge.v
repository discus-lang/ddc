
Require Export Exp.
Require Export SubstValueValue.


(* Small Step Evaluation ********************************************)
Inductive STEP : exp -> exp -> Prop :=
 | ESLamApp
   : forall t11 x12 v2
   ,  value v2
   -> STEP (XApp   (XLam t11 x12) v2)
           (subst v2 x12)

 | ESApp1 
   :  forall x1 x1' x2
   ,  STEP x1 x1'
   -> STEP (XApp x1 x2) (XApp x1' x2)

 | ESApp2 
   :  forall v1 x2 x2'
   ,  value v1
   -> STEP x2 x2'
   -> STEP (XApp v1 x2) (XApp v1 x2').

Hint Constructors STEP.







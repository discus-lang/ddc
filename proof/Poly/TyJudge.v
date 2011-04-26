
Require Import SubstTypeType.
Require Import KiJudge.
Require Import Exp.
Require Import Env.
Require Import Base.


(* Check well typeness of terms. *)
Inductive TYPE : kienv -> tyenv -> exp -> ty -> Prop :=
 | TYVar 
   :  forall i ke te t
   ,  get te i = Some t
   -> TYPE ke te (XVar i) t

 | TYLam 
   :  forall ke te x12 t11 t12
   ,  TYPE ke (te :> t11)  x12            t12
   -> TYPE ke  te         (XLam t11 x12) (TFun t11 t12)

 | TYApp 
   :  forall ke te x1 x2 t11 t12
   ,  TYPE ke te x1 (TFun t11 t12) 
   -> TYPE ke te x2 t11
   -> TYPE ke te (XApp x1 x2) t12

 | TYLAM
   :  forall ke te x1 t1
   ,  TYPE (ke :> KStar) (liftTE te) x1        t1
   -> TYPE ke            te          (XLAM x1) (TForall t1)

 | TYAPP
   :  forall ke te x1 t1 t2
   ,  TYPE ke te x1 (TForall t1)
   -> KIND ke t2 KStar
   -> TYPE ke te (XAPP x1 t2) (substTT t2 t1). 

Hint Constructors TYPE.




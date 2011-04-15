
Require Export Exp.

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



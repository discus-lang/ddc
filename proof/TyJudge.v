
Require Export Base.
Require Export Name.
Require Export Context.
Require Export Ty.
Require Export Exp.

Definition tyenv := partial_map ty.

Inductive TYPE : tyenv -> exp -> ty -> Prop :=
 | TYVar 
   :  forall env x T
   ,  env x = some T
   -> TYPE env (XVar x) T

 | TYLam 
   :  forall env x T11 T12 t12
   ,  TYPE (extend env x T11) t12 T12
   -> TYPE env                (XLam x T11 t12) (TFun T11 T12)

 | TYApp 
   :  forall env t1 t2 T11 T12
   ,  TYPE env t1 (TFun T11 T12)
   -> TYPE env t2 T11
   -> TYPE env (XApp t1 t2) T12.

Tactic Notation "TYPE_cases" tactic(first) ident(c) :=
 first;
 [ Case_aux c "TYVar"
 | Case_aux c "TYLam"
 | Case_aux c "TYApp"].

Hint Constructors TYPE.
Hint Unfold  beq_name beq_nat extend.
Hint Resolve extend_eq. 






 
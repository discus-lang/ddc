
Require Export Exp.
Require Export SubstExpExp.


(** Evaluation contexts of expressions *)
Inductive exp_ctx : (exp -> exp) -> Prop :=
 | XcTop
   :  exp_ctx (fun x => x)

 | XcApp1
   :  forall x2
   ,  exp_ctx (fun xx => XApp xx x2)

 | XcApp2 
   :  forall v1
   ,  value v1 
   -> exp_ctx (fun xx => XApp v1 xx).

Hint Constructors exp_ctx.


(** Single Small Step Evaluation using contexts *)
Inductive STEPc : exp -> exp -> Prop :=

 (* Evaluation in a context *)
 | EsContext 
   :  forall C x x'
   ,  exp_ctx C
   -> STEPc x x'
   -> STEPc (C x) (C x')

 | EsLamApp 
   :  forall t11 x12 v2
   ,  value v2
   -> STEPc (XApp (XLam t11 x12) v2)
            (substX 0 v2 x12).

Hint Constructors STEPc.


Require Import DDC.Language.SystemF2Store.TyJudge.
Require Export DDC.Data.Context.
Require Export DDC.Data.Chain.


(********************************************************************)
(*  Evaluation contexts for expressions.
    This describes a place in the exp AST where the sub-expression
    there is able to take an evaluation step *)
Inductive exp_ctx : (exp -> exp) -> Prop :=

 (* The top level context names the entire expression *)
 | XcTop 
   : exp_ctx  (fun x => x)

 (* Left of an application *)
 | XcApp1
   :  forall x2
   ,  exp_ctx  (fun xx => XApp xx x2)

 (* The right of an application can step only when the left is
    already a value. *)
 | XcApp2 
   :  forall v1
   ,  value v1
   -> exp_ctx  (fun xx => XApp v1 xx)

 | XcAPP
   :  forall t2
   ,  exp_ctx  (fun xx => XAPP xx t2)

 (* As the XCon constructor contains a list of sub-expressions, 
    we need an additional exps_ctx context to indicate which one 
    we're talking about. *)
 | XcCon 
   :  forall dc ts C
   ,  exps_ctx wnfX C 
   -> exp_ctx  (fun xx => XCon dc ts (C xx))

 (* We need to reduce the discriminant of a case to a value. *)
 | XcCase
   :  forall alts
   ,  exp_ctx  (fun xx => XCase xx alts)

 (* Reduce the object to be updated *)
 | XcUpdate1 
   :  forall cn i tsParam xField
   ,  exp_ctx  (fun xx => XUpdate cn i tsParam xx xField)

 (* Reduce the new field value *)
 | XcUpdate2 
   :  forall cn i tsParam vObj
   ,  value vObj
   -> exp_ctx  (fun xx => XUpdate cn i tsParam vObj xx).

Hint Constructors exp_ctx. 


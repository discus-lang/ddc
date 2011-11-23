
Require Export DDC.Language.SimplePCFa.Exp.
Require Export DDC.Language.SimplePCFa.Ty.


Inductive TYPEX : tyenv -> exp -> ty -> Prop :=
 | TxVar
   :  forall te i t
   ,  get i te = Some t
   -> TYPEX te (XVal (VVar i)) t

 | TxConstBool
   :  forall te b
   ,  TYPEX te (XVal (VConst (CBool b))) tBool

 | TxConstNat
   :  forall te n
   ,  TYPEX te (XVal (VConst (CNat n))) tNat

 | TxLam
   :  forall te t1 t2 x2
   ,  TYPEX (te :> t1) x2 t2
   -> TYPEX te (XVal (VLam t1 x2)) (TFun t1 t2)

 | TxFix
   :  forall te t1 v2
   ,  TYPEX (te :> t1) (XVal v2) t1
   -> TYPEX te (XVal (VFix t1 v2)) t1

 | TxLet
   :  forall te t1 x1 t2 x2
   ,  TYPEX (te :> t1) x2 t2
   -> TYPEX te        (XLet t1 x1 x2) t2

 | TxApp
   :  forall te t11 t12 v1 v2
   ,  TYPEX te (XVal v1) (TFun t11 t12) 
   -> TYPEX te (XVal v2) t11
   -> TYPEX te (XApp v1 v2) t12

 | TxOpSucc
   :  forall te v1
   ,  TYPEX te (XVal v1) tNat
   -> TYPEX te (XOp1 OSucc v1) tNat

 | TxOpPred
   :  forall te v1
   ,  TYPEX te (XVal v1) tNat
   -> TYPEX te (XOp1 OPred v1) tNat

 | TxOpIsZero
   :  forall te v1
   ,  TYPEX te (XVal v1) tNat
   -> TYPEX te (XOp1 OIsZero v1) tBool

 | TxIf
   :  forall te v1 x2 x3 tR
   ,  TYPEX te (XVal v1) tBool
   -> TYPEX te x2 tR
   -> TYPEX te x3 tR
   -> TYPEX te (XIf v1 x2 x3) tR.
Hint Constructors TYPEX.


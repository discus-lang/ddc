
Require Import DDC.Language.SystemF2Effect.KiJudge.
Require Import DDC.Language.SystemF2Effect.TySubst.
Require Import DDC.Language.SystemF2Effect.VaExp.


(* Types of Value expressions *)
Inductive TYPEV : tyenv -> val -> ty -> Prop := 
  | TvVar
    :  forall te i t
    ,  get i te = Some t
    -> TYPEV te (VVar i) t 

  | TvLam
    :  forall te t1 t2 x2 e2
    ,  TYPEX (te :> t1) x2 t2 e2
    -> TYPEV te (VLam t1 x2) (tFun t1 e2 t2)

  (* TODO: LAM *)

  | TvAPP
    :  forall te v1 k11 t12 t2
    ,  TYPEV te  v1 (TForall k11 t12)
    -> KIND  nil t2 k11
    -> TYPEV te  (VAPP v1 t2) (substTT 0 t2 t12)

  | TvConstNat
    :  forall te n
    ,  TYPEV te (VConst (CNat n))  tNat

  | TvConstBool
    :  forall te b
    ,  TYPEV te (VConst (CBool b)) tBool


  with TYPEX : tyenv -> exp -> ty -> ty -> Prop :=
  | TxVal
    :  forall te v1 t1
    ,  TYPEV te v1        t1
    -> TYPEX te (XVal v1) t1 (TBot KEffect)

  | TxLet
    :  forall te t1 x1 t2 x2 e1 e2
    ,  TYPEX te         x1 t1 e1
    -> TYPEX (te :> t1) x2 t2 e2
    -> TYPEX te        (XLet t1 x1 x2) t2 (TSum e1 e2)

  | TxApp
    :  forall te t11 t12 v1 v2 e1
    ,  TYPEV te v1 (tFun t11 e1 t12) 
    -> TYPEV te v2 t11
    -> TYPEX te (XApp v1 v2) t12 e1

  (* Unary Operators *)
  | TxOpSucc
    :  forall te v1
    ,  TYPEV te v1 tNat
    -> TYPEX te (XOp1 OSucc v1)   tNat  (TBot KEffect)

  | TxOpPred
    :  forall te v1
    ,  TYPEV te v1 tNat
    -> TYPEX te (XOp1 OPred v1)   tNat  (TBot KEffect)

  (* TODO: Alloc *)

  (* Binary Operators *)
  | TxOpRead
    :  forall te v1 r1 t2
    ,  TYPEV te v1 (tRef r1 t2)
    -> TYPEX te (XOp1 ORead v1)   t2      (tRead r1)

  | TxOpWrite
    :  forall te v1 v2 r1 t2
    ,  TYPEV te v1 (tRef r1 t2)
    -> TYPEV te v2 t2
    -> TYPEX te (XOp2 OWrite v1 v2) tUnit (tWrite r1).


Hint Constructors TYPEV.
Hint Constructors TYPEX.

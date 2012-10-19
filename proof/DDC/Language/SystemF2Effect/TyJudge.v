
Require Import DDC.Language.SystemF2Effect.KiJudge.
Require Import DDC.Language.SystemF2Effect.TySubst.
Require Import DDC.Language.SystemF2Effect.VaExp.


(* Store Environment holds the types of locations. *)
Definition stenv := list ty.


(* Types of Value expressions *)
Inductive TYPEV : kienv -> tyenv -> stenv -> val -> ty -> Prop := 
  | TvVar
    :  forall ke te se i t
    ,  get i te = Some t
    -> TYPEV ke te se (VVar i) t 

  | TvLam
    :  forall ke te se t1 t2 x2 e2
    ,  TYPEX ke (te :> t1) se x2 t2 e2
    -> TYPEV ke te se (VLam t1 x2) (tFun t1 e2 t2)

  | TvLAM
    :  forall ke te se k1 t2 x2
    ,  TYPEX (ke :> k1) te se x2 t2 (TBot KEffect)
    -> TYPEV ke te se (VLAM k1 x2) (TForall k1 t2)

  | TvAPP
    :  forall ke te se v1 k11 t12 t2
    ,  TYPEV ke te se v1 (TForall k11 t12)
    -> KIND  ke t2 k11
    -> TYPEV ke te se (VAPP v1 t2) (substTT 0 t2 t12)

  | TvConstNat
    :  forall ke te se n
    ,  TYPEV ke te se (VConst (CNat n))  tNat

  | TvConstBool
    :  forall ke te se b
    ,  TYPEV ke te se (VConst (CBool b)) tBool


  with TYPEX : kienv -> tyenv -> stenv -> exp -> ty -> ty -> Prop :=
  | TxVal
    :  forall ke te se v1 t1
    ,  TYPEV ke te se v1        t1
    -> TYPEX ke te se (XVal v1) t1 (TBot KEffect)

  | TxLet
    :  forall ke te se t1 x1 t2 x2 e1 e2
    ,  TYPEX ke te         se x1 t1 e1
    -> TYPEX ke (te :> t1) se x2 t2 e2
    -> TYPEX ke te         se (XLet t1 x1 x2) t2 (TSum e1 e2)

  | TxApp
    :  forall ke te se t11 t12 v1 v2 e1
    ,  TYPEV ke te se v1 (tFun t11 e1 t12) 
    -> TYPEV ke te se v2 t11
    -> TYPEX ke te se (XApp v1 v2) t12 e1

  (* Unary Operators *)
  | TxOpSucc
    :  forall ke te se v1
    ,  TYPEV ke te se v1 tNat
    -> TYPEX ke te se (XOp1 OSucc v1) tNat (TBot KEffect)

  | TxOpPred
    :  forall ke te se v1
    ,  TYPEV ke te se v1 tNat
    -> TYPEX ke te se (XOp1 OPred v1) tNat (TBot KEffect)

  | TxOpAlloc 
    : forall ke te se r1 v2 t2
    ,  KIND  ke r1 KRegion
    -> TYPEV ke te se v2 t2
    -> TYPEX ke te se (XOp1 (OAlloc r1) v2) (tRef r1 t2) (tAlloc r1)

  | TxOpRead
    :  forall ke te se v1 r1 t2
    ,  TYPEV ke te se v1 (tRef r1 t2)
    -> TYPEX ke te se (XOp1 ORead v1)   t2      (tRead r1)

  (* Binary Operators *)
  | TxOpWrite
    :  forall ke te se v1 v2 r1 t2
    ,  TYPEV ke te se v1 (tRef r1 t2)
    -> TYPEV ke te se v2 t2
    -> TYPEX ke te se (XOp2 OWrite v1 v2) tUnit (tWrite r1).

Hint Constructors TYPEV.
Hint Constructors TYPEX.

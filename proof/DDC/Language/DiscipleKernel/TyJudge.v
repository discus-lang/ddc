
Require Import DDC.Language.DiscipleKernel.KiJudge.
Require Import DDC.Language.DiscipleKernel.TySubst.
Require Import DDC.Language.DiscipleKernel.VaExp.


Inductive TYPEW : kienv -> tyenv -> wit -> ty -> Prop :=
  (* Witness Expressions *)
  | TwVar
    :  forall ke te i t
    ,  get i te = Some t
    -> TYPEW ke te (WVar i) t

  | TwApp
    :  forall ke te w1 w2 t11 t12
    ,  TYPEW ke te w1 (tImpl t11 t12)
    -> TYPEW ke te w2 t11
    -> TYPEW ke te (WApp w1 w2) t12

  | TwAPP
    :  forall ke te w1 k12 t12 t2
    ,  TYPEW ke te w1 (TForall k12 t12)
    -> KIND  ke t2 k12
    -> TYPEW ke te (WAPP w1 t2) (substTT 0 t2 t12)

  | TwJoinPure
    :  forall ke te w1 w2 e1 e2
    ,  TYPEW ke te w1 (tPure e1)
    -> TYPEW ke te w2 (tPure e2)
    -> TYPEW ke te (WJoin w1 w2) (tPure (TSum e1 e2))

 | TwJoinEmpty
    :  forall ke te w1 w2 e1 e2
    ,  TYPEW ke te w1 (tEmpty e1)
    -> TYPEW ke te w2 (tEmpty e2)
    -> TYPEW ke te (WJoin w1 w2) (tEmpty (TSum e1 e2))

  (* TODO: Capabilities*)


  (* Witness Constructors *)
  | TwConPure 
    :  forall ke te
    ,  TYPEW ke te (WCon IPure)  (tPure  (TBot KEffect))

  | TwConEmpty
    :  forall ke te
    ,  TYPEW ke te (WCon IEmpty) (tEmpty (TBot KClosure))

  | TwConRead
    :  forall ke te r
    ,  TYPEW ke te (WCon IRead)  (tImpl (tConst r) (tPure (tRead r)))

  | TwConAlloc
    :  forall ke te r
    ,  TYPEW ke te (WCon IAlloc) (tImpl (tConst r) (tPure (tAlloc r))).
Hint Constructors TYPEW.


Inductive TYPEV : tyenv -> val -> ty -> Prop := 
  | TvVar
    :  forall te i t
    ,  get i te = Some t
    -> TYPEV te (VVar i) t 

  | TvFix
    :  forall te t1 v2
    ,  TYPEV (te :> t1) v2 t1
    -> TYPEV te (VFix t1 v2) t1

  | TvLam
    :  forall te t1 t2 x2 e2
    ,  TYPEX (te :> t1) x2 t2 e2
    -> TYPEV te (VLam t1 x2) (tFun t1 e2 (TBot KClosure) t2)

  | TvSat
    :  forall te v1 w2 t11 t12
    ,  TYPEW nil te  w2 t11 
    -> TYPEV te      v1           (tImpl t11 t12)
    -> TYPEV te     (VSat v1 w2)  t12

  | TvAPP
    :  forall te v1 k11 t12 t2
    ,  TYPEV te  v1 (TForall k11 t12)
    -> KIND  nil t2 k11
    -> TYPEV te  (VAPP v1 t2) (substTT 0 t2 t12)

  | TvConstBool
    :  forall te b
    ,  TYPEV te (VConst (CBool b)) tBool

  | TvConstNat
    :  forall te n
    ,  TYPEV te (VConst (CNat n)) tNat


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
    ,  TYPEV te v1 (tFun t11 e1 (TBot KClosure) t12) 
    -> TYPEV te v2 t11
    -> TYPEX te (XApp v1 v2) t12 e1

 (* Branching *)
  | TxIf
    :  forall te v1 x2 x3 tR e1 e2
    ,  TYPEV te v1 tBool
    -> TYPEX te x2 tR e1
    -> TYPEX te x3 tR e2
    -> TYPEX te (XIf v1 x2 x3) tR (TSum e1 e2)

 (* Unary Operators *)
  | TxOpSucc
    :  forall te v1
    ,  TYPEV te v1 tNat
    -> TYPEX te (XOp1 OSucc v1)   tNat  (TBot KEffect)

  | TxOpPred
    :  forall te v1
    ,  TYPEV te v1 tNat
    -> TYPEX te (XOp1 OPred v1)   tNat  (TBot KEffect)

  | TxOpIsZero
    :  forall te v1
    ,  TYPEV te v1 tNat
    -> TYPEX te (XOp1 OIsZero v1) tBool (TBot KEffect).
Hint Constructors TYPEV.
Hint Constructors TYPEX.

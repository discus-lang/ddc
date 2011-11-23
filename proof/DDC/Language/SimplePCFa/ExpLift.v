
Require Import DDC.Language.SimplePCFa.Exp.

(* Lifting of references into the environment *)
Fixpoint liftXV (d: nat) (vv: val) : val := 
  match vv with
  | VVar i       => if le_gt_dec d i then VVar (S i) else vv
  | VConst c     => VConst c
  | VLam t x     => VLam t (liftXX (S d) x)
  | VFix t v     => VFix t (liftXV (S d) v)
  end

with    liftXX (d: nat) (xx: exp) : exp :=
  match xx with
  | XVal v       => XVal   (liftXV d v)
  | XLet t x1 x2 => XLet t (liftXX d x1) (liftXX (S d) x2)
  | XApp v1 v2   => XApp   (liftXV d v1) (liftXV d v2)
  | XOp1 o v     => XOp1 o (liftXV d v)
  | XIf v1 x2 x3 => XIf    (liftXV d v1) (liftXX d x2) (liftXX d x3)
  end.



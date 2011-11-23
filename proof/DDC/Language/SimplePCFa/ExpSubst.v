
Require Import DDC.Language.SimplePCFa.Exp.
Require Import DDC.Language.SimplePCFa.ExpLift.


(* Substitute for the outer-most binder in an expression *)
Fixpoint substVV (d: nat) (u: val) (vv: val) :=
  match vv with
  | VVar ix 
  => match nat_compare ix d with
     | Eq  => u
     | Gt  => VVar (ix - 1)
     | Lt  => VVar ix
     end
  | VConst c     => VConst c
  | VLam t x     => VLam t (substVX (S d) (liftXV 0 u) x)
  | VFix t v     => VFix t (substVV (S d) (liftXV 0 u) v)
  end

with    substVX (d: nat) (u: val) (xx: exp) :=
  match xx with
  | XVal v       => XVal   (substVV d u v)
  | XLet t x1 x2 => XLet t (substVX d u x1) (substVX (S d) u x2)
  | XApp v1 v2   => XApp   (substVV d u v1) (substVV d u v2)
  | XOp1 o v     => XOp1 o (substVV d u v)
  | XIf v1 x2 x3 => XIf    (substVV d u v1) (substVX d u x2) (substVX d u x3)
  end.


Require Import DDC.Language.SimplePCFa.Exp.


(* Value references some expression in the environment *)
Fixpoint refsXV (ix: nat) (vv: val) : Prop
 := match vv with
    | VVar ix' 
    => match nat_compare ix ix' with
       | Eq  => True
       | _   => False
       end
    | VConst c     => False
    | VFun t x     => refsXX (S (S ix)) x
    end
with    refsXX (ix: nat) (xx: exp) :=
  match xx with
  | XVal v       => refsXV ix v
  | XLet t x1 x2 => refsXX ix x1 \/ refsXX (S ix) x2
  | XApp v1 v2   => refsXV ix v1 \/ refsXV ix v2
  | XOp1 o v     => refsXV ix v
  | XIf v1 x2 x3 => refsXV ix v1 \/ refsXX ix x2 \/ refsXX ix x3
  end.



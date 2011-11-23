
Require Import DDC.Language.SimplePCFa.Exp.
Require Import DDC.Language.SimplePCFa.ExpLift.


(* Lowering of references into the environment *)
Fixpoint lowerXV (d: nat) (vv: val) : val := 
  match vv with
  | VVar    O    => vv
  | VVar (S i)   => if le_gt_dec d (S i) then VVar i else vv
  | VConst c     => VConst c
  | VLam t x     => VLam t (lowerXX (S d) x)
  | VFix t v     => VFix t (lowerXV (S d) v)
  end

with    lowerXX (d: nat) (xx: exp) : exp :=
  match xx with
  | XVal v       => XVal   (lowerXV d v)
  | XLet t x1 x2 => XLet t (lowerXX d x1) (lowerXX (S d) x2)
  | XApp v1 v2   => XApp   (lowerXV d v1) (lowerXV d v2)
  | XOp1 o v     => XOp1 o (lowerXV d v)
  | XIf v1 x2 x3 => XIf    (lowerXV d v1) (lowerXX d x2) (lowerXX d x3)
  end.


Lemma lowerXX_liftXX_id
 : forall d x
 , lowerXX d (liftXX d x) = x.
Proof.
 intros. gen d.
 induction x using exp_mutind with
  (PV := fun v => forall d, lowerXV d (liftXV d v) = v); 
  intros; simpl; try burn.

  SCase "VVar".
    repeat (simpl; lift_cases; try burn).
   destruct n;
    repeat (simpl; lift_cases; try burn).
Qed.
    
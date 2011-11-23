

Require Import DDC.Language.SimplePCFa.Exp.
Require Import DDC.Language.SimplePCFa.ExpLift.

(* Substitution of values in expressions and values *)
Definition substVVs' (ix: nat) (d: nat) (vs: list val) (vv: val)
 := match get (ix - d) vs with
    | Some u => u
    | None   => VVar (ix - length vs)
   end.


Fixpoint substVVs (d: nat) (vs: list val) (vv: val) 
 := match vv with 
    |  VVar ix
    => match nat_compare ix d with
       | Lt  => VVar ix  (* ix is locally bound *)
       | _   => substVVs' ix d vs vv
       end

    |  VConst c
    => vv

    |  VLam t x
    => VLam t (substVXs (S d) (map (fun v => liftXV 0 v) vs) x)

    |  VFix t v
    => VFix t (substVVs (S d) (map (fun v => liftXV 0 v) vs) v)
    end
with    substVXs (d: nat) (vs: list val) (xx: exp)
 := match xx with
    |  XVal v 
    => XVal (substVVs d vs v)

    |  XLet t x1 x2
    => XLet t (substVXs d vs x1)
              (substVXs (S d) (map (liftXV 0) vs) x2)
    
    |  XApp v1 v2
    => XApp (substVVs d vs v1) (substVVs d vs v2)

    |  XOp1 op v1
    => XOp1 op (substVVs d vs v1)

    |  XIf v1 x2 x3
    => XIf (substVVs d vs v1) (substVXs d vs x2) (substVXs d vs x3)
    end.


(* Closed substitution of values in expressions and values *)
Definition csubstVXs (vs: list val) (xx: exp) (xx': exp)
 := xx' = substVXs 0 vs xx
 /\ closedX xx'.




Require Export DDC.Language.SystemF2Effect.TySubst.
Require Export DDC.Language.SystemF2Effect.VaExpBase.
Require Export DDC.Language.SystemF2Effect.VaExpLift.


(* Substitution of Types in Exps *)
Fixpoint substTV (d: nat) (u: ty) (vv: val) : val :=
  match vv with
  | VVar _       => vv
  | VLoc _       => vv
  | VLam t x     => VLam (substTT d u t) (substTX d u x)
  | VLAM k x     => VLAM k (substTX (S d) (liftTT 1 0 u) x)
  | VConst c     => vv
  end
 with    substTX (d: nat) (u: ty) (xx: exp) : exp :=
  match xx with
  | XVal v       => XVal   (substTV d u v)
  | XLet t x1 x2 => XLet   (substTT d u t)  (substTX d u x1) (substTX d u x2)
  | XApp v1 v2   => XApp   (substTV d u v1) (substTV d u v2)
  | XAPP v1 t2   => XAPP   (substTV d u v1) (substTT d u t2)

  | XAlloc t1 v2 => XAlloc (substTT d u t1) (substTV d u v2)
  | XRead  v1    => XRead  (substTV d u v1)
  | XWrite v1 v2 => XWrite (substTV d u v1) (substTV d u v2)

  | XOp1 op1 v   => XOp1   op1 (substTV d u v)
  end.  


(* Substitution of Values in Exps *)
Fixpoint substVV (d: nat) (u: val) (vv: val) : val :=
  match vv with
  | VVar ix 
  => match nat_compare ix d with
     (* Index matches the one we are substituting for. *)
     | Eq  => u
     
     (* Index was free in the original expression.
        As we've removed the outermost binder, also decrease this
        index by one. *)
     | Gt  => VVar (ix - 1)

     (* Index was bound in the original expression. *)
     | Lt  => VVar ix
     end

  | VLoc l         => vv

  (* Increase the depth as we move across a lambda.
     Also lift free references in the exp being substituted
     across the lambda as we enter it. *)
  | VLam t1 x2    => VLam t1 (substVX (S d) (liftXV 1 0 u) x2)

  | VLAM k1 x2    => VLAM k1 (substVX d (liftTV 0 u) x2)

  | VConst c      => VConst c
  end
 with   substVX (d: nat) (u: val) (xx: exp) : exp :=
  match xx with
  |  XVal v       => XVal (substVV d u v)

  |  XLet t1 x2 x3 
  => XLet t1 (substVX d u x2) (substVX (S d) (liftXV 1 0 u) x3)

  |  XApp v1 v2   => XApp   (substVV d u v1) (substVV d u v2)
  |  XAPP v1 t2   => XAPP   (substVV d u v1) t2

  |  XAlloc t1 v2 => XAlloc t1 (substVV d u v2)
  |  XRead  v1    => XRead  (substVV d u v1)
  |  XWrite v1 v2 => XWrite (substVV d u v1) (substVV d u v2)

  |  XOp1 op v1   => XOp1 op (substVV d u v1)
  end.


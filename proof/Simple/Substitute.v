
Require Export Exp.

(** Substitution ******************************************)
Fixpoint liftX (n: nat) (depth: nat) (tt: exp) : exp :=
 match tt with 
 | XVar ix    => if bge_nat ix depth
                  then XVar (ix + n)
                  else tt

 | XLam T1 t1 => XLam T1 (liftX n (depth + 1) t1)

 | XApp t1 t2 => XApp (liftX n depth t1)
                      (liftX n depth t2)
 end.


Fixpoint subLocalX' (depth: nat) (u: exp) (tt: exp)  : exp :=
 match tt with
 | XVar ix    =>  match compare ix depth with
                  | EQ => liftX depth 0 u
                  | GT => XVar (ix - 1)
                  | _  => XVar ix
                  end

 | XLam T1 t2 => XLam T1 (subLocalX' (S depth) u t2)

 | XApp t1 t2 => XApp (subLocalX' depth u t1)
                      (subLocalX' depth u t2)
 end. 


Definition  subLocalX := subLocalX' 0.
Hint Unfold subLocalX.


(** Lemmas **********************************************************)

(* Lifting an expression by 0 steps doesn't do anything *)
Theorem liftX_none
 : forall t1 depth
 , liftX 0 depth t1 = t1.
Proof.
 induction t1; intro; simpl.

 Case "XVar".
  assert (n + 0 = n). omega. rewrite H.
  breaka (bge_nat n depth).

 Case "XLam".
  rewrite IHt1. auto.

 Case "XApp". 
  rewrite IHt1_1. rewrite IHt1_2. auto.
Qed.


Theorem liftX_covers 
 : forall ix n t
 , coversX n t -> liftX ix n t = t.
Proof.
 intros ix n t.
 gen n.
 induction t; intros.
 rename n0 into n'.
 Case "XVar".
  simpl. breaka (bge_nat n n').
  inversions H. apply bge_nat_true in HeqX. 
  contradict H2. omega.

 Case "XLam".
  simpl. inversions H.
  rewrite IHt. auto.
  assert (S n = n + 1). omega. rewrite <- H. auto.

 Case "XApp".
  simpl. inversions H.
  rewrite IHt1. rewrite IHt2. auto. auto. auto.
Qed.


(* If a term is closed then lifting it doesn't do anything *)
Theorem liftX_closed
 : forall ix t
 , closedX t -> liftX ix 0 t = t.
Proof.
 intros.
 apply liftX_covers.
 inversions H. auto.
Qed.




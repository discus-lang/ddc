
Require Export DDC.Language.SystemF2.TyBase.


(*******************************************************************)
(* Lift type indices that are at least a certain depth. *)
Fixpoint liftTT (n: nat) (d: nat) (tt: ty) : ty :=
  match tt with
  |  TVar ix
  => if le_gt_dec d ix
      then TVar (ix + n)
      else tt

  |  TCon _     => tt

  |  TForall t 
  => TForall (liftTT n (S d) t)

  |  TApp t1 t2
  => TApp    (liftTT n d t1) (liftTT n d t2)
  end.
Hint Unfold liftTT.


(* Tactic to help deal with lifting functions *)
Ltac lift_cases 
 := match goal with 
     |  [ |- context [le_gt_dec ?n ?n'] ]
     => case (le_gt_dec n n')
    end.


(********************************************************************)
(* Changing the order of lifting. *)
Lemma liftTT_liftTT
 :  forall d d' t
 ,  liftTT 1 d              (liftTT 1 (d + d') t) 
 =  liftTT 1 (1 + (d + d')) (liftTT 1 d t).
Proof.
 intros. gen d d'.
 induction t; intros; simpl; try burn.

 Case "TVar".
  repeat (unfold liftTT; lift_cases; intros); burn.

 Case "TForall".
  assert (S (d + d') = (S d) + d'). omega. rewrite H. 
  rewrite IHt. auto.
Qed.  


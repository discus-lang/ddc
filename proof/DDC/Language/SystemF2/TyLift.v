
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
Lemma getCtorOfType_liftTT
 :  forall d ix t 
 ,  getCtorOfType (liftTT d ix t) = getCtorOfType t.
Proof.
 intros.
 induction t; try burn.

 Case "TVar".
  simpl. lift_cases; auto.
Qed.  


Lemma liftTT_takeTCon
 :  forall tt d ix
 ,  liftTT d ix (takeTCon tt) = takeTCon (liftTT d ix tt).
Proof.
 intros.
 induction tt; intros; auto.
 simpl. lift_cases; auto.
Qed.


Lemma liftTT_takeTArgs
 : forall tt d ix
 , map (liftTT d ix) (takeTArgs tt) = takeTArgs (liftTT d ix tt).
Proof.
 intros.
 induction tt; intros; auto.
 simpl. lift_cases; auto.
 simpl. rewrite map_snoc. 
  f_equal. auto.
Qed. 


Lemma liftTT_makeTApps 
 :  forall n d t1 ts
 ,  liftTT n d (makeTApps t1 ts)
 =  makeTApps (liftTT n d t1) (map (liftTT n d) ts). 
Proof.
 intros. gen t1.
 induction ts; intros.
  auto.
  simpl. rewrite IHts. auto.
Qed.


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

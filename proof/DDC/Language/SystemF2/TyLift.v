
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


(********************************************************************)
(* Lifting and well-formedness *)
Lemma liftTT_wfT
 :  forall kn t d
 ,  wfT kn t
 -> wfT (S kn) (liftTT 1 d t).
Proof.
 intros. gen kn d.
 lift_burn t.
 
 Case "TVar".
  inverts H.
  repeat (simpl; lift_cases).
   eapply WfT_TVar. burn.
   eapply WfT_TVar. burn.

 Case "TForall".
  inverts H. burn.

 Case "TApp".
  inverts H. burn.
Qed.
Hint Resolve liftTT_wfT.


(********************************************************************)
(* Lifting and type utils *)
Lemma liftTT_getCtorOfType
 :  forall d ix t 
 ,  getCtorOfType (liftTT d ix t) = getCtorOfType t.
Proof.
 lift_burn t.
Qed.  
Hint Rewrite liftTT_getCtorOfType : global.


Lemma liftTT_takeTCon
 :  forall tt d ix
 ,  liftTT d ix (takeTCon tt) = takeTCon (liftTT d ix tt).
Proof.
 intros; lift_burn tt.
Qed.
Hint Rewrite liftTT_takeTCon : global.


Lemma liftTT_takeTCon'
 :  forall n d tt tCon
 ,  takeTCon tt = tCon
 -> takeTCon (liftTT n d tt) = liftTT n d tCon.
Proof.
 intros. gen n d.
 induction tt; intros; rewrite <- H; burn.
Qed.
Hint Resolve liftTT_takeTCon'.


Lemma liftTT_takeTArgs
 : forall tt d ix
 , map (liftTT d ix) (takeTArgs tt) = takeTArgs (liftTT d ix tt).
Proof.
 intros.
 induction tt; intros; auto.
 simpl. lift_cases; auto.
 simpl. rewrite map_snoc. burn. 
Qed. 
Hint Rewrite liftTT_takeTArgs : global.


Lemma liftTT_takeTArgs'
 :  forall n d tt tsArgs
 ,  takeTArgs tt = tsArgs
 -> takeTArgs (liftTT n d tt) = map (liftTT n d) tsArgs.
Proof.
 intros.
 induction tt; intros; rewrite <- H; burn.
Qed.
Hint Resolve liftTT_takeTArgs'.


Lemma liftTT_makeTApps
 :  forall n d t1 ts
 ,  liftTT n d (makeTApps t1 ts)
 =  makeTApps (liftTT n d t1) (map (liftTT n d) ts). 
Proof.
 intros. gen t1.
 induction ts; burn.
Qed.
Hint Rewrite liftTT_makeTApps : global.


(********************************************************************)
Lemma liftTT_zero
 :  forall d t
 ,  liftTT 0 d t = t.
Proof.
 intros. gen d.
 lift_burn t.
Qed.
Hint Rewrite liftTT_zero : global.


Lemma liftTT_comm
 :  forall n m d t
 ,  liftTT n d (liftTT m d t)
 =  liftTT m d (liftTT n d t).
Proof.
 intros. gen d.
 lift_burn t.
Qed.


Lemma liftTT_succ
 :  forall n m d t
 ,  liftTT (S n) d (liftTT m     d t)
 =  liftTT n     d (liftTT (S m) d t).
Proof.
 intros. gen d m n.
 lift_burn t.
Qed.
Hint Rewrite liftTT_succ : global. 


Lemma liftTT_plus
 : forall n m d t
 , liftTT n d (liftTT m d t) = liftTT (n + m) d t.
Proof.
 intros. gen n d.
 induction m; intros.
 rewrite liftTT_zero; burn.

 rrwrite (n + S m = S n + m). 
  rewrite liftTT_comm.
  rewrite <- IHm.
  rewrite liftTT_comm.
  burn.
Qed. 
Hint Rewrite <- liftTT_plus : global.


Lemma liftTT_wfT_1
 :  forall t n ix
 ,  wfT n t
 -> liftTT 1 (n + ix) t = t.
Proof.
 intros. gen n ix.
 induction t; intros; auto.
  Case "TVar".
   inverts H. simpl. lift_cases; burn.

  Case "TForall".
   simpl. f_equal.
   inverts H.
   eapply IHt in H1.
   rrwrite (S (n + ix) = S n + ix).
   burn.

  Case "TApp".
   inverts H.
   repeat rewritess; burn.
Qed.


(********************************************************************)
(* Changing the order of lifting.
   We build this up in stages. 
   Start out by only allow lifting by a single place for both
   applications. Then allow lifting by multiple places in the first
   application, then multiple places in both. 
*)
Lemma liftTT_liftTT_11
 :  forall d d' t
 ,  liftTT 1 d              (liftTT 1 (d + d') t) 
 =  liftTT 1 (1 + (d + d')) (liftTT 1 d t).
Proof.
 intros. gen d d'.
 induction t; intros; simpl; try burn.

 Case "TVar".
  repeat (lift_cases; unfold liftTT); burn.

 Case "TForall".
  rrwrite (S (d + d') = (S d) + d').
  rewrite IHt. burn.
Qed.  


Lemma liftTT_liftTT_1
 :  forall n1 m1 n2 t
 ,  liftTT m1   n1 (liftTT 1 (n2 + n1) t)
 =  liftTT 1 (m1 + n2 + n1) (liftTT m1 n1 t).
Proof.
 intros. gen n1 m1 n2 t.
 induction m1; intros; simpl.
  burn. 

  rrwrite (S m1 = 1 + m1).
  rewrite <- liftTT_plus.
  rewrite IHm1.
  rrwrite (m1 + n2 + n1 = n1 + (m1 + n2)).
  rewrite liftTT_liftTT_11.
  burn.
Qed.


Lemma liftTT_liftTT
 :  forall m1 n1 m2 n2 t
 ,  liftTT m1 n1 (liftTT m2 (n2 + n1) t)
 =  liftTT m2 (m1 + n2 + n1) (liftTT m1 n1 t).
Proof.
 intros. gen n1 m1 n2 t.
 induction m2; intros.
  burn.

  rrwrite (S m2 = 1 + m2).
  rewrite <- liftTT_plus.
  rewrite liftTT_liftTT_1.
  rewrite IHm2.
  burn.
Qed.
Hint Rewrite liftTT_liftTT : global.


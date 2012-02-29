
Require Export DDC.Language.DiscipleKernel.TyExp.
Require Export DDC.Language.DiscipleKernel.TyWfT.


(*******************************************************************)
(* Lift type indices that are at least a certain depth. *)
Fixpoint liftTT (n: nat) (d: nat) (tt: ty) : ty :=
 match tt with
 |  TVar ix
 => if le_gt_dec d ix
     then TVar (ix + n)
     else tt

 |  TCon _      => tt
 |  TForall k t => TForall k (liftTT n (S d) t)
 |  TApp t1 t2  => TApp      (liftTT n d t1) (liftTT n d t2)
 |  TSum t1 t2  => TSum      (liftTT n d t1) (liftTT n d t2)
 |  TBot k      => TBot k
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
 lift_burn t; inverts H; try burn.
 
 Case "TVar".
  repeat (simpl; lift_cases).
   eapply WfT_TVar. burn.
   eapply WfT_TVar. burn.
Qed.
Hint Resolve liftTT_wfT.


(********************************************************************)
Lemma liftTT_zero
 :  forall d t
 ,  liftTT 0 d t = t.
Proof.
 intros. gen d. lift_burn t.
Qed.
Hint Rewrite liftTT_zero : global.


Lemma liftTT_comm
 :  forall n m d t
 ,  liftTT n d (liftTT m d t)
 =  liftTT m d (liftTT n d t).
Proof.
 intros. gen d. lift_burn t.
Qed.


Lemma liftTT_succ
 :  forall n m d t
 ,  liftTT (S n) d (liftTT m     d t)
 =  liftTT n     d (liftTT (S m) d t).
Proof.
 intros. gen d m n. lift_burn t.
Qed.
Hint Rewrite liftTT_succ : global. 


Lemma liftTT_plus
 : forall n m d t
 , liftTT n d (liftTT m d t) = liftTT (n + m) d t.
Proof.
 intros. gen n d.
 induction m; intros.
 rewrite liftTT_zero; burn.

 rw (n + S m = S n + m). 
  rewrite liftTT_comm.
  rewrite <- IHm.
  rewrite liftTT_comm.
  burn.
Qed. 
Hint Rewrite <- liftTT_plus : global.


(******************************************************************************)
Lemma liftTT_wfT_1
 :  forall t n ix
 ,  wfT n t
 -> liftTT 1 (n + ix) t = t.
Proof.
 intros. gen n ix.
 induction t; intros; inverts H; simpl; auto.

  Case "TVar".
   lift_cases; burn.

  Case "TForall".
   f_equal. spec IHt H1.
   rw (S (n + ix) = S n + ix).
   burn.

  Case "TApp".
   rs. burn.

  Case "TSum".
   rs. burn.
Qed.
Hint Resolve liftTT_wfT_1.


Lemma liftTT_closedT_id_1
 :  forall t d
 ,  closedT t
 -> liftTT 1 d t = t.
Proof.
 intros.
 rw (d = d + 0). eauto.
Qed.
Hint Resolve liftTT_closedT_id_1.


Lemma liftTT_closedT_10
 :  forall t
 ,  closedT t
 -> closedT (liftTT 1 0 t).
Proof.
 intros. red.
 rw (0 = 0 + 0).
 rewrite liftTT_wfT_1; auto.
Qed.
Hint Resolve liftTT_closedT_10.


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
  rw (S (d + d') = (S d) + d').
  burn.
Qed.


Lemma liftTT_liftTT_1
 :  forall n1 m1 n2 t
 ,  liftTT m1   n1 (liftTT 1 (n2 + n1) t)
 =  liftTT 1 (m1 + n2 + n1) (liftTT m1 n1 t).
Proof.
 intros. gen n1 m1 n2 t.
 induction m1; intros; simpl.
  burn. 

  rw (S m1 = 1 + m1).
  rewrite <- liftTT_plus.
  rs.
  rw (m1 + n2 + n1 = n1 + (m1 + n2)).
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

  rw (S m2 = 1 + m2).
  rewrite <- liftTT_plus.
  rewrite liftTT_liftTT_1.
  rewrite IHm2.
  burn.
Qed.
Hint Rewrite liftTT_liftTT : global.


Lemma liftTT_map_liftTT
 :  forall m1 n1 m2 n2 ts
 ,  map (liftTT m1 n1) (map (liftTT m2 (n2 + n1)) ts)
 =  map (liftTT m2 (m1 + n2 + n1)) (map (liftTT m1 n1) ts).
Proof.
 induction ts; simpl; burn.
Qed.  


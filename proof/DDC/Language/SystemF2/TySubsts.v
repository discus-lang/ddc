
Require Export DDC.Language.SystemF2.TyBase.
Require Export DDC.Language.SystemF2.TyLift.
Require Export DDC.Language.SystemF2.TySubst.
Require Import Coq.Logic.FunctionalExtensionality.


(********************************************************************)
Definition substTTs' (ix: nat) (d: nat) (us: list ty) (tt: ty)
 := match get (ix - d) us with
    | Some u => u
    | None   => TVar (ix - length us)
   end.

Fixpoint substTTs (d: nat) (us: list ty) (tt: ty)
 := match tt with
   |  TVar ix
   => match nat_compare ix d with
      | Lt => TVar ix   (* ix is locally bound *)
      | _  => substTTs' ix d us tt
      end

   |  TCon _
   => tt
   
   |  TForall t
   => TForall (substTTs (S d) (map (liftTT 1 0) us) t)

   |  TApp t1 t2
   => TApp (substTTs d us t1) (substTTs d us t2)
   end.


(********************************************************************)
Lemma substTTs_nil
 :  forall n t
 ,  substTTs n nil t = t.
Proof.
 intros. gen n.
 induction t; intros; auto.

 Case "TVar".
  simpl. lift_cases; auto;
   unfold substTTs'; nnat;
    lift_cases; burn.

 Case "TForall".
  simpl. burn.

 Case "TApp".
  repeat rewritess.
Qed.


Lemma substTTs_makeTApps 
 :  forall d us t1 ts
 ,  substTTs d us (makeTApps t1 ts) 
 =  makeTApps (substTTs d us t1) (map (substTTs d us) ts).
Proof.
 intros. gen d us t1.
 induction ts; intros.
  auto.
  simpl. rewrite IHts. auto.
Qed.


(********************************************************************)
(* Lifting after substitution *)
Lemma liftTT_substTTs_1
 :  forall n n' t1 us
 ,  liftTT 1 n (substTTs (n + n') us t1)
 =  substTTs (1 + n + n') (map (liftTT 1 n) us) (liftTT 1 n t1).
Proof.
 intros. gen n n' us.
 induction t1; intros.

 Case "TCon".
  auto.

 Case "TVar".
  repeat (simpl; lift_cases);
   induction us;
    unfold substTTs'; 
     auto;
     try (rewrite map_length);
     try (simpl; lift_cases; burn).

  SCase "manual case".
   repeat simpl.
   rrwrite (n0 + n' - (n0 + n') = 0).
   rrwrite (n0 + n' + 1 - S (n0 + n') = 0). 
   auto.

  SCase "manual case".
   repeat simpl.
   nnat.
   rrwrite  (S n - S (n0 + n') = n - (n0 + n')).
   remember (n - (n0 + n')) as X.
   destruct X.
    auto.
    rewrite (match_option (liftTT 1 n0)).
    simpl.
    remember (get X (map (liftTT 1 n0) us)) as Y.
    destruct Y. 
     auto. 
     symmetry in HeqY.
     apply get_none_length in HeqY.
     rewrite map_length in HeqY.
     lift_cases; 
      intros; fequal; burn.

 Case "TForall".
  simpl. f_equal. 
  induction us.
   simpl. 
    rrwrite (S (n + n') = (S n) + n').
    rewrite IHt1. auto.
   simpl.
    rewrite (liftTT_liftTT_11 0 n). simpl.
    rewrite (IHt1 (S n) n'). 
    f_equal. simpl. f_equal.
     lists. f_equal. extensionality x.
     symmetry.
     rrwrite (n = n + 0).
     rewrite liftTT_liftTT. auto.

 Case "TApp".
  simpl. f_equal; rewritess.
Qed.


Lemma liftTT_substTTs
 :  forall m n n' t1 us
 ,  liftTT m n (substTTs (n + n') us t1)
 =  substTTs (m + n + n') (map (liftTT m n) us) (liftTT m n t1).
Proof.
 intros. gen n n'.
 induction m; intros.
  simpl.
   rewrite liftTT_zero.
   rewrite liftTT_zero.
   f_equal.
   rewrite (@map_impl ty ty (liftTT 0 n) id).
    induction us; auto. simpl. rewrite <- IHus. auto.
   intros. rewrite liftTT_zero. unfold id. auto.

  simpl.
  rrwrite (S m = 1 + m).
  rewrite <- liftTT_plus.
  rewrite IHm.
  simpl.
  rrwrite (m + n + n' = n + (m + n')).
  rewrite liftTT_substTTs_1.
  f_equal. 
   lists. f_equal.
    extensionality x.
    rewrite -> liftTT_plus. auto.
   rewrite -> liftTT_plus. auto.
Qed.



(********************************************************************)
(* Lifting after substitution, 
   with the lifting at a higher index *)
Lemma liftTT_substTTs_nonempty_env
 :  forall n n' t1 us
 ,  length us > 0
 -> liftTT 1 (n' + n) (substTTs n us t1)
 =  substTTs n (map (liftTT 1 (n' + n)) us) 
               (liftTT 1 (length us + n' + n) t1).
Proof.
 intros. gen n n' us.
 induction t1; intros.

 Case "TCon".
  simpl. auto.

 Case "TVar".
   repeat (simpl; unfold substTTs'; lift_cases);
    try auto;
    try (false; omega);
    try (lists; burn).

 Case "TForall".
  simpl. f_equal.
  rrwrite (S (n' + n) = n' + S n).
  erewrite IHt1.
  f_equal.
   lists. f_equal. extensionality x.
    rrwrite (n' + S n = 1 + (n' + n) + 0).
    rewrite <- liftTT_liftTT. nnat. auto.
   f_equal; lists; omega.
  lists; omega.

 Case "TApp".
  simpl. f_equal.
  rewrite IHt1_1; eauto.
  rewrite IHt1_2; eauto.
Qed.



Lemma liftTT_substTTs'
 :  forall n n' t1 us
 ,  liftTT 1 (n' + n) (substTTs n us t1)
 =  substTTs n (map (liftTT 1 (n' + n)) us) 
               (liftTT 1 (length us + n' + n) t1).
Proof.
 intros.
 destruct us.
  simpl. 
   rewrite substTTs_nil.
   rewrite substTTs_nil. auto.
  simpl.
  apply liftTT_substTTs_nonempty_env.
  simpl. omega.
Qed.


Lemma liftTT_map_substTT
 :  forall m n n' ts t2
 ,  map (liftTT m n) (map (substTT (n + n') t2) ts)
 =  map (substTT (m + n + n') (liftTT m n t2)) (map (liftTT m n) ts).
Proof.
 intros.
 induction ts.
  auto.
  simpl. rewrite IHts. f_equal.
  rewrite liftTT_substTT. auto.
Qed.


Lemma substTT_closed'
 :  forall t1 t2 ix tn m
 ,  wfT tn t1
 -> ix >= tn
 -> substTT ix (liftTT 1 m t2) t1 = t1.
Proof.
 intros. gen t2 ix tn m.
 induction t1; intros; auto.

 Case "TVar".
  simpl.
  lift_cases; auto.
   inverts H. burn.
   inverts H. burn.

 Case "TForall".
  simpl. f_equal.
   erewrite IHt1. eauto.
    inverts H. eauto. omega.

 Case "TApp".
  simpl. inverts H.
   erewrite IHt1_1; eauto.
   erewrite IHt1_2; eauto.
Qed.


Lemma substTT_closed
 :  forall ix t1 t2
 ,  closedT t1
 -> substTT ix t2 t1 = t1.
Proof.
 intros.
 assert (exists n, wfT n t2).
  auto. dest n.

 assert (exists n, liftTT 1 n t2 = t2).
  exists n. eauto.
  rrwrite (n = n + 0). 
  apply liftTT_wfT_1. auto.
  destruct H1 as [n'].

 rewrite <- H1.
 eapply substTT_closed'.
 unfold closedT in H. eauto. omega.
Qed.


Lemma substTTs_closing'
 :  forall ts t1 tn n
 ,  Forall (wfT (tn + n)) ts
 -> wfT (length ts + n) t1
 -> wfT (tn + n) (substTTs n ts t1).
Proof.
 intros. gen n tn ts.
 induction t1; intros; unfold closedT.
  simpl. auto.

 Case "TVar".
  inverts H0.
  simpl; lift_cases; eauto.
   unfold substTTs'.
    lift_cases. 
      admit. (* ok, get in*)
      eapply WfT_TVar. lists. omega.
    eapply WfT_TVar.
      omega.
   unfold substTTs'.
    lift_cases.
     admit. (* ok, get in *)
     eapply WfT_TVar. lists. omega.

 Case "TForall".
  inverts H0. simpl.
  eapply WfT_TForall.
  rrwrite (S (tn + n) = tn + (S n)).
  eapply IHt1.
   eapply Forall_map. 
   nforall. intros. admit. (* ok, lemma *)
   lists.
   assert (S (length ts + n) = length ts + (S n)).
    omega.
    rewrite H0 in H2. eauto.

 Case "TApp".
  inverts H0.
  simpl. eauto. 
Qed.
Hint Resolve substTTs_closing'.


Lemma substTTs_closing
 :  forall tn ts t1
 ,  Forall (wfT tn) ts
 -> wfT (length ts) t1
 -> wfT tn (substTTs 0 ts t1).
Proof.
 intros.
 rrwrite (tn = tn + 0).
 eapply substTTs_closing';
  nnat; auto.
Qed.


Lemma substTTs_substTT
 :   forall n n' t1 t2 ts
 ,   wfT (length ts) t1
 ->  substTTs n (map (substTT (n + n') t2) ts) t1
 =   substTT (n + n') t2 (substTTs n ts t1).
Proof.
 intros.
  induction t1; intros; auto.
  admit.

 Case "TForall".
  inverts H.
  simpl. f_equal.
  rrwrite (n + n' = 0 + (n + n')). 
  rewrite liftTT_map_substTT. nnat.
  rrwrite (1 + (n + n') = S n + n').
  rrwrite (S (0 + (n + n')) = 1 + n + n'). nnat.
  lists.
  rrwrite (S n + n' = 1 + 0 + (n + n')).
  admit.
  (* hmm *)

 Case "TApp".
  inverts H. simpl. burn.
Qed.

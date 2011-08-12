

Require Export DDC.Language.SystemF2.TyBase.
Require Export DDC.Language.SystemF2.TyLift.
Require Import Coq.Logic.FunctionalExtensionality.


Ltac fbreak_le_gt_dec'
 := match goal with 
     |  [ |- context [le_gt_dec ?n ?n'] ]
     => case (le_gt_dec n n')
    end.


Ltac lift_cases' := 
 repeat (intros;
  first [ fbreak_nat_compare
        | fbreak_le_gt_dec'
        | fbreak_get]); intros.


Lemma get_some_get_map_some
 :  forall {A} t1 t2 ix us (f: A -> A)
 ,  Some t1 = get ix us
 -> Some t2 = get ix (map f us)
 -> f t1 = t2.
Proof.
 intros. gen ix t1 t2.
 induction us; intros.
  false.
  destruct ix.
   simpl in H. simpl in H0. 
   inverts H.  inverts H0. auto.

   simpl in H. simpl in H0; eauto.
Qed.


Ltac make_get_some_get_map_some
 := match goal with
    | [ H1 : Some ?t1 = get _ _
      , H2 : Some ?t2 = get _ _
      |- _]  
      => lets D: @get_some_get_map_some ty H1 H2; eauto
    end.

Ltac make_get_some_none_lengths
 := match goal with
    | [ H1 : Some ?t1 = get _ _
      , H2 : None     = get _ _
      |- _]  
      => symmetry in H1; apply get_length_more in H1;
         symmetry in H2; apply get_none_length in H2
    end.


Lemma get_zero_nonempty_some
 :  forall {A} (us: list A)
 ,  length us > 0
 -> (exists x, get 0 us = Some x).
Proof.
 intros.
 destruct us.
  simpl in H. inverts H.
  simpl. eauto.
Qed.



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
  simpl. f_equal. auto.

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
Lemma match_option
 : forall {A} {B} (f: A -> B) (xs: list A) ix y
 , f (match get ix xs         with | Some x => x | None  => y   end)
 =   (match get ix (map f xs) with | Some x => x | None  => f y end).
Proof.
 intros.
 remember (get ix xs) as Get.
 destruct Get.
  symmetry in HeqGet.
  apply (get_map f) in HeqGet.
  rewrite HeqGet. auto.

  admit. (* fine, list lemma *)
Qed.



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
    rewrite map_map. rewrite map_map.
    f_equal. unfold compose. 
    extensionality x.
    symmetry.
    rrwrite (n = n + 0).
    rewrite liftTT_liftTT. auto.

 Case "TApp".
  simpl. f_equal.
  rewritess. rewritess.
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
  rewrite map_map. unfold compose.
  f_equal.
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
   repeat (simpl; unfold substTTs'; lift_cases');
    try (rewrite map_length);
    try auto;
    try (false; omega); 
    try make_get_some_get_map_some;
    try (make_get_some_none_lengths);
    try (rewrite map_length in HeqH1; burn).

   SCase "manual case".
    f_equal. 
     apply get_zero_nonempty_some in H. dest x.
     assert (n0 - n0 = 0). omega.
      rewrite H0 in HeqH0.
      rewrite H in HeqH0. false.

   SCase "manual case".
    symmetry in HeqH0. apply get_length_more in HeqH0.
    symmetry in HeqH1. apply get_length_more in HeqH1.
    rewrite map_length in HeqH1. burn.

   SCase "manual case".
    symmetry in HeqH0. apply get_none_length in HeqH0.
    symmetry in HeqH1. apply get_none_length in HeqH1.
    rewrite map_length in HeqH1. burn.

   SCase "manual case".
    symmetry in HeqH0. apply get_none_length in HeqH0.
    symmetry in HeqH1. apply get_none_length in HeqH1.
    rewrite map_length in HeqH1. burn.

 Case "TForall".
  simpl. f_equal.
  rrwrite (S (n' + n) = n' + S n).
  erewrite IHt1.
  f_equal.
   rewrite map_map.
   rewrite map_map.
   unfold compose. f_equal. extensionality x.
   rrwrite (n' + S n = 1 + (n' + n) + 0).
   rewrite <- liftTT_liftTT. nnat. auto.
  f_equal.
   rewrite map_length. omega.
   rewrite map_length. omega.

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


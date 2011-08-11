

Require Export DDC.Language.SystemF2.TyBase.
Require Export DDC.Language.SystemF2.TyLift.
Require Import Coq.Logic.FunctionalExtensionality.


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


Ltac fbreak_get 
 := match goal with 
    |  [ |- context [get ?E1 ?E2] ] 
    => case (get E1 E2)
    end.

Ltac fbreak_le_gt_dec
 := match goal with 
     |  [ |- context [le_gt_dec ?n ?n'] ]
     => case (le_gt_dec n n')
    end.


Ltac lift_unfold := 
 repeat (intros; simpl;
  first [ fbreak_nat_compare
        | fbreak_le_gt_dec
        | fbreak_get]).


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


Lemma get_none_length
 :  forall {A} n (xs: list A)
 ,  get n xs = None
 -> length xs <= n.
Proof.
 intros. gen n.
 induction xs; intros.
  simpl. burn.
  simpl. simpl in H.
   destruct n.
    false.
    apply IHxs in H. omega.
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
  lift_unfold; 
   induction us; 
    unfold substTTs';
    try lift_unfold; intros; simpl;
    try (rewrite map_length);
    try burn.

  SCase "manual case".
   rrwrite (n0 + n' - (n0 + n') = 0).
   rrwrite (n0 + n' + 1 - S (n0 + n') = 0). 
   auto.

  SCase "manual case".
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
     lift_unfold; 
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
(*
Lemma liftTT_substTTs'
 :  forall n n' t1 us
 ,  length us > 0
 -> liftTT 1 (n + n') (substTTs n us t1)
 =  substTTs n (map (liftTT 1 (n + n')) us) (liftTT 1 (length us + n + n') t1).
Proof.
 intros. gen n n' us.
 induction t1; intros.

 Case "TCon".
  simpl. auto.

 Case "TVar".

  repeat ( unfold liftTT; unfold substTTs; unfold substTTs';
           fold   liftTT; fold substTTs; fold substTTs'
         ; try fbreak_le_gt_dec
         ; try fbreak_nat_compare
         ; try fbreak_get
         ; intros)
    ; try (rewrite map_length)
    ; try burn.

 admit. admit. admit.
*)
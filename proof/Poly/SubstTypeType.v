
Require Import KiJudge.
Require Import WellFormed.
Require Import Exp.
Require Import Base.


(* Types ************************************************************)
(* Lift type indices that are at least a certain depth. *)
Fixpoint liftTT' (d: nat) (tt: ty) : ty :=
  match tt with
  | TCon _     => tt

  |  TVar ix
  => if le_gt_dec d ix
      then TVar (S ix)
      else tt

  |  TForall t 
  => TForall (liftTT' (S d) t)

  |  TFun t1 t2
  => TFun    (liftTT' d t1) (liftTT' d t2)
  end.
Hint Unfold liftTT'.

Definition liftTT  := liftTT' 0.
Hint Unfold liftTT.

Ltac liftTT_cases 
 := match goal with 
     |  [ |- context [le_gt_dec ?n ?n'] ]
     => case (le_gt_dec n n')
    end.


(* Substitution of Types in Types. *)
Fixpoint substTT' (d: nat) (u: ty) (tt: ty) : ty 
 := match tt with
    |  TCon _     
    => tt
 
    | TVar ix
    => match nat_compare ix d with
       | Eq => u
       | Gt => TVar (ix - 1)
       | _  => TVar  ix
       end

    |  TForall t  
    => TForall (substTT' (S d) (liftTT' 0 u) t)

    |  TFun t1 t2 
    => TFun (substTT' d u t1) (substTT' d u t2)
  end.

Definition substTT := substTT' 0.
Hint Unfold substTT.


(* Type Environments ************************************************)
(* Lift type indices in type environments. *)
Definition liftTE' d te    := map (liftTT' d) te.
Hint Unfold liftTE'.

Definition liftTE te       := liftTE' 0 te.
Hint Unfold liftTE.

(* Substitution of Types in Type Environments. *)
Definition substTE' d t te := map (substTT' d t) te.
Hint Unfold substTE'.

Definition substTE te      := substTE' 0 te.
Hint Unfold substTE.


(* Lifting Lemmas ***************************************************)
(* Changing the order of lifting. *)
Lemma liftTT_liftTT
 :  forall n n' t
 ,  liftTT' n              (liftTT' (n + n') t) 
 =  liftTT' (1 + (n + n')) (liftTT' n t).
Proof.
 intros. gen n n'.
 induction t; intros; auto.

 Case "TVar".
  simpl.
  repeat (unfold liftTT'; liftTT_cases; intros); burn.

 Case "TForall".
  simpl.
  assert (S (n + n') = (S n) + n'). omega. rewrite H. 
  rewrite IHt. auto.

 Case "TFun".
  simpl. apply f_equal2; auto.
Qed.  


(* If we lift at depth d, this creates an empty space and
   substituting into it doens't do anything. *)
Lemma substTT_liftTT
 :  forall d t1 t2
 ,  substTT' d t2 (liftTT' d t1) = t1.
Proof.
 intros. gen d t2.
 induction t1; intros; eauto.

 Case "TVar".
  simpl; liftTT_cases; unfold substTT';
   fbreak_nat_compare; intros;
   burn.

 Case "TForall".
  simpl. 
  rewrite IHt1. auto.

 Case "TFun".
  simpl.
  rewrite IHt1_1.
  rewrite IHt1_2. auto.
Qed.


(* Lifting after substitution *)
Lemma liftTT_substTT
 :  forall n n' t1 t2
 ,  liftTT' n (substTT' (n + n') t2 t1)
 =  substTT' (1 + n + n') (liftTT' n t2) (liftTT' n t1).
Proof.
 intros. gen n n' t2.
 induction t1; intros; eauto.

 Case "TVar".
  repeat (simpl; fbreak_nat_compare; 
          try liftTT_cases; try intros);
   burn.

 Case "TForall".
  simpl.
  rewrite (IHt1 (S n) n'). simpl.
  rewrite (liftTT_liftTT 0 n). auto.

 Case "TFun".
  simpl.
  rewrite IHt1_1. auto.
  rewrite IHt1_2. auto.
Qed.


(* Commuting substitutions. *)
Lemma substTT_substTT
 :  forall n m t1 t2 t3
 ,  substTT' (n + m) t3 (substTT' n t2 t1)
 =  substTT' n (substTT' (n + m) t3 t2)
               (substTT' (1 + n + m) (liftTT' n t3) t1).
Proof.
 intros. gen n m t2 t3.
 induction t1; intros; auto.

 Case "TVar".
  repeat (simpl; fbreak_nat_compare; burn).
  rewrite substTT_liftTT. auto.

 Case "TForall".
  simpl. f_equal.
  rewrite (IHt1 (S n) m). f_equal.
   simpl. rewrite (liftTT_substTT 0 (n + m)). auto.
   simpl. rewrite (liftTT_liftTT 0 n). auto.  

 Case "TFun".
  simpl. f_equal.
   apply IHt1_1.
   apply IHt1_2.
Qed.


(* Lifting lemmas on environments ***********************************)
Lemma liftTE_substTE
 :  forall n n' t2 te
 ,  liftTE' n (substTE' (n + n') t2 te)
 =  substTE' (1 + n + n') (liftTT' n t2) (liftTE' n te).
Proof.
 intros. induction te.
  auto.
  unfold substTE'. unfold liftTE'.
   simpl. rewrite liftTT_substTT.
   unfold liftTE' in IHte.
   unfold substTE' in IHte. rewrite IHte. auto.
Qed.


(* Weakening Kind environment ***************************************)
Lemma liftTT_insert
 :  forall ke ix t k1 k2
 ,  KIND ke t k1
 -> KIND (insert ix k2 ke) (liftTT' ix t) k1.
Proof.
 intros. gen ix ke k1.
 induction t; intros; simpl; inverts H; eauto.

 Case "TVar".
  liftTT_cases; intros; auto.

 Case "TForall".
  apply KIForall.
  rewrite insert_rewind.
  apply IHt. auto.
Qed.


Lemma liftTT_push
 :  forall ke t k1 k2
 ,  KIND  ke         t         k1
 -> KIND (ke :> k2) (liftTT t) k1.
Proof.
 intros.
 assert (ke :> k2 = insert 0 k2 ke). simpl.
   destruct ke; auto.
 rewrite H0. apply liftTT_insert. auto.
Qed.


(* Theorems *********************************************************)
(* Substitution of types in types preserves kinding.
   Must also subst new new type into types in env higher than ix
   otherwise indices ref subst type are broken.
   Resulting type env would not be well formed *)

Theorem subst_type_type_ix
 :  forall ix ke t1 k1 t2 k2
 ,  get ke ix = Some k2
 -> KIND ke t1 k1
 -> KIND (drop ix ke) t2 k2
 -> KIND (drop ix ke) (substTT' ix t2 t1) k1.
Proof.
 intros. gen ix ke t2 k1 k2.
 induction t1; intros; simpl; inverts H0; eauto.

 Case "TVar".
  destruct k1. destruct k2.
  fbreak_nat_compare.
  SCase "n = ix".
   auto.

  SCase "n < ix".
   apply KIVar. rewrite <- H4.
   apply get_drop_above; auto.

  SCase "n > ix".
   apply KIVar. rewrite <- H4.
   destruct n.
    burn.
    simpl. nnat. apply get_drop_below. omega.

 Case "TForall".
  apply KIForall.
  rewrite drop_rewind.
  eapply IHt1; eauto.
   apply liftTT_push. auto.
Qed.


Theorem subst_type_type
 :  forall ke t1 k1 t2 k2
 ,  KIND (ke :> k2) t1 k1
 -> KIND ke         t2 k2
 -> KIND ke (substTT t2 t1) k1.
Proof.
 intros.
 unfold substTT.
 assert (ke = drop 0 (ke :> k2)). auto. rewrite H1.
 eapply subst_type_type_ix; simpl; eauto.
Qed.


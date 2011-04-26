
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
    => TForall (substTT' (S d) (liftTT u) t)

    |  TFun t1 t2 
    => TFun (substTT' d u t1) (substTT' d u t2)
  end.

Definition substTT := substTT' 0.
Hint Unfold substTT.


Ltac lift_cases :=
  match goal with 
  |   |- ?x 
  => match x with 
     |  context [le_gt_dec ?n ?n'] => case (le_gt_dec n n')
     end
  end.


(* Type Environments ************************************************)
(* Lift type indices in type environments. *)
Definition liftTE' d    := map (liftTT' d).
Hint Unfold liftTE'.

Definition liftTE       := liftTE' 0.
Hint Unfold liftTE.

(* Substitution of Types in Type Environments. *)
Definition substTE' d t := map (substTT' d t). 
Hint Unfold substTE'.

Definition substTE      := substTE' 0.
Hint Unfold substTE.


(* Lifting Lemmas ***************************************************)

(* Changing the order of lifting. 
     example indices: 0 1 2 3 4 5
          let n + n': 1 + 2

   Lift by (n + n') then by (n).
     lift >= 3:       0 1 2 4 5 6
     lift >= 1:       0 2 3 5 6 7  ** same result

   Lift by (n) then by (1 + (n + n'))
     lift >= 1:       0 2 3 4 5 6
     lift >= 4:       0 2 3 5 6 7  ** same result
 *)
Lemma liftTT_liftTT
 :  forall n n' t
 ,  liftTT' n              (liftTT' (n + n') t) 
 =  liftTT' (1 + (n + n')) (liftTT' n t).
Proof.
 intros. gen n n'.
 induction t; intros.
 Case "TCon".
  simpl. auto.

 Case "TVar".
  simpl. unfold liftTT'.
  repeat lift_cases; intros; 
   auto; 
   try (false; omega).

 Case "TForall".
  simpl. apply f_equal.
  assert (S (n + n') = (S n) + n'). omega. rewrite H. clear H.
  rewrite IHt. simpl. auto.

 Case "TFun".
  simpl. apply f_equal2; auto.
Qed.  


Lemma liftTT_insert
 :  forall ke ix t k1 k2
 ,  KIND ke t k1
 -> KIND (insert ix k2 ke) (liftTT' ix t) k1.
Proof.
 intros. gen ix ke k1.
 induction t; intros; simpl; inverts H; eauto.

 Case "TVar".
  lift_cases; intros; auto.

 Case "TForall".
  apply KIForall. rewrite insert_rewind. apply IHt. auto.
Qed.


Lemma liftTT_push
 :  forall ke t k1 k2
 ,  KIND  ke         t         k1
 -> KIND (ke :> k2) (liftTT t) k1.
Proof.
 intros.
 assert (ke :> k2 = insert 0 k2 ke). simpl. destruct ke; auto.
 rewrite H0. apply liftTT_insert. auto.
Qed.


(* Theorems *********************************************************)
(* Substitution of types in types preserves kinding.
   Must also subst new new type into types in env higher than ix
   otherwise indices ref subst type are broken.
   Resulting type env would not be well formed *)

Theorem subst_type_type_drop
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
   apply KIVar. rewrite <- H4. apply get_drop_above; auto.

  SCase "n > ix".
   apply KIVar. rewrite <- H4.
   destruct n.
    false. omega.
    simpl. nnat. apply get_drop_below. omega.

 Case "TForall".
  apply KIForall. rewrite drop_rewind.
  eapply IHt1. auto. simpl. eauto. 
  simpl. apply liftTT_push. auto.
Qed.


Theorem subst_type_type
 :  forall ke t1 k1 t2 k2
 ,  KIND (ke :> k2) t1 k1
 -> KIND ke         t2 k2
 -> KIND ke (substTT t2 t1) k1.
Proof.
 intros.
 assert (ke = drop 0 (ke :> k2)). auto. rewrite H1. clear H1.
 unfold substTT.
 eapply subst_type_type_drop; simpl; eauto.
Qed.


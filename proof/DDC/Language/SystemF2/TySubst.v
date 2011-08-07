
Require Export DDC.Language.SystemF2.TyBase.
Require Export DDC.Language.SystemF2.TyLift.
Require Import Coq.Logic.FunctionalExtensionality.


(********************************************************************)
(* Substitution of Types in Types. *)
Fixpoint substTT (d: nat) (u: ty) (tt: ty) : ty 
 := match tt with
    | TVar ix
    => match nat_compare ix d with
       | Eq => u
       | Gt => TVar (ix - 1)
       | _  => TVar  ix
       end

    |  TCon _     
    => tt
 
    |  TForall t  
    => TForall (substTT (S d) (liftTT 1 0 u) t)

    |  TApp t1 t2 
    => TApp (substTT d u t1) (substTT d u t2)
  end.


Fixpoint substTTs (d: nat) (us: list ty) (tt: ty) :=
 match us with
 | nil      => tt
 | u :: us' => substTTs d us' 
                 (substTT d (liftTT (List.length us') 0 u)
                            tt)
 end.


(********************************************************************)
Lemma substTT_makeTApps 
 :  forall d t2 t1 ts
 ,  substTT d t2 (makeTApps t1 ts) 
 =  makeTApps (substTT d t2 t1) (map (substTT d t2) ts).
Proof.
 intros. gen d t2 t1.
 induction ts; intros.
  auto.
  simpl. rewrite IHts. auto.
Qed.


(********************************************************************)
(* If we lift at depth d, this creates an empty space and
   substituting into it doens't do anything. *)
Lemma substTT_liftTT
 :  forall d t1 t2
 ,  substTT d t2 (liftTT 1 d t1) = t1.
Proof.
 intros. gen d t2.
 induction t1; intros; simpl; try burn.

 Case "TVar".
  lift_cases; unfold substTT;
   fbreak_nat_compare; intros;
   burn.
Qed.


(* Lifting after substitution *)
Lemma liftTT_substTT1
 :  forall n n' t1 t2
 ,  liftTT 1 n (substTT (n + n') t2 t1)
 =  substTT (1 + n + n') (liftTT 1 n t2) (liftTT 1 n t1).
Proof.
 intros. gen n n' t2.
 induction t1; intros; simpl; try burn.

 Case "TVar".
  repeat (simpl; fbreak_nat_compare; 
          try lift_cases; try intros);
   burn.

 Case "TForall".
  simpl.
  rewrite (IHt1 (S n) n'). simpl.
  rewrite (liftTT_liftTT' 0 n). auto.

 Case "TApp".
  rewrite IHt1_1. 
  rewrite IHt1_2. auto.
Qed.


Lemma liftTT_substTT
 :  forall m n n' t1 t2
 ,  liftTT m n (substTT (n + n') t2 t1)
 =  substTT (m + n + n') (liftTT m n t2) (liftTT m n t1).
Proof.
 intros. gen n n'.
 induction m; intros.
  simpl.  
   rewrite liftTT_zero.
   rewrite liftTT_zero.
   rewrite liftTT_zero.
   auto.
  simpl.
   assert (S m = 1 + m). 
    omega. rewrite H.
   rewrite <- liftTT_plus.
   rewrite IHm.
   assert (m + n + n' = n + (m + n')).
    omega. rewrite H0.
   rewrite liftTT_substTT1.
   simpl. 
   f_equal; rewrite liftTT_plus; auto.
Qed.


Lemma substTTs_TForall
 :  forall ts d t
 ,  substTTs d ts (TForall t)
 =  TForall (substTTs (1 + d) (map (liftTT 1 0) ts) t).
Proof.
 intros. gen d t.
 induction ts; intros.
  simpl. auto.
  simpl.
  rewrite map_length.
   rewrite <- IHts.
   f_equal. f_equal. f_equal.
   rewrite liftTT_comm. auto.
Qed.

(*
Lemma liftTT_substTTs
 :  forall m n n' ts t
 ,  liftTT m n (substTTs (n + n') ts t)
 =  substTTs (m + n + n') (map (liftTT m n) ts) (liftTT m n t).
Proof.
 intros. gen m n n' ts.
 induction t; intros.

 Case "TCon".
  induction ts.
   auto.
   simpl. rewrite IHts. auto.

 Case "TVar".
   admit.

 Case "TForall".
  simpl.
  rewrite substTTs_TForall. simpl.
  rewrite substTTs_TForall. simpl.
  f_equal.
  assert (S (n + n') = S n + n').
   omega. rewrite H.
  rewrite IHt.
  f_equal. omega. simpl.
  rewrite map_map. rewrite map_map.
  unfold compose.
  f_equal.
   extensionality x.
   simpl.

*)

Lemma liftTT_substTT'
 :  forall n n' t1 t2
 ,  liftTT 1 (n + n') (substTT n t2 t1)
 =  substTT n (liftTT 1 (n + n') t2) (liftTT 1 (1 + n + n') t1).
Proof.
 intros. gen n n' t2.
 induction t1; intros; eauto.

 Case "TVar".
  repeat ( unfold liftTT; unfold substTT; fold liftTT; fold substTT
         ; try lift_cases; try fbreak_nat_compare
         ; intros); try burn.

 Case "TForall".
  simpl. f_equal.
  rewrite (IHt1 (S n) n'). f_equal.
   simpl. rewrite (liftTT_liftTT' 0 (n + n')). auto.

 Case "TApp".
  simpl. f_equal.
   apply IHt1_1.
   apply IHt1_2.
Qed.


(* Commuting substitutions. *)
Lemma substTT_substTT
 :  forall n m t1 t2 t3
 ,  substTT (n + m) t3 (substTT n t2 t1)
 =  substTT n (substTT (n + m) t3 t2)
              (substTT (1 + n + m) (liftTT 1 n t3) t1).
Proof.
 intros. gen n m t2 t3.
 induction t1; intros; auto.

 Case "TVar".
  repeat (simpl; fbreak_nat_compare); try burn.
  rewrite substTT_liftTT. auto.

 Case "TForall".
  simpl. f_equal.
  rewrite (IHt1 (S n) m). f_equal.
   simpl. rewrite (liftTT_substTT1 0 (n + m)). auto.
   simpl. rewrite (liftTT_liftTT' 0 n). auto.  

 Case "TApp".
  simpl. f_equal.
   apply IHt1_1.
   apply IHt1_2.
Qed.

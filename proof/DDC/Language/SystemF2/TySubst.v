
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


(********************************************************************)
(* Lifting after substitution,
   with the lifting at a lower index. *)
Lemma liftTT_substTT_1
 :  forall n n' t1 t2
 ,  liftTT 1 n (substTT (n + n') t2 t1)
 =  substTT (1 + n + n') (liftTT 1 n t2) (liftTT 1 n t1).
Proof.
 intros. gen n n' t2.
 induction t1; intros; simpl; try burn; try (repeat rewritess).

 Case "TVar".
  repeat (simpl; fbreak_nat_compare; 
          try lift_cases; try intros); burn.

 Case "TForall".
  rewrite (IHt1 (S n) n').
  rewrite (liftTT_liftTT_11 0 n). auto.
Qed.


Lemma liftTT_substTT
 :  forall m n n' t1 t2
 ,  liftTT m n (substTT (n + n') t2 t1)
 =  substTT (m + n + n') (liftTT m n t2) (liftTT m n t1).
Proof.
 intros. gen n n'.
 induction m; intros; simpl.
  repeat (rewrite liftTT_zero); auto.

  rrwrite (S m = 1 + m).
  rewrite <- liftTT_plus.
  rewrite IHm.
  rrwrite (m + n + n' = n + (m + n')).
  rewrite liftTT_substTT_1. 
  simpl; f_equal; rewrite liftTT_plus; auto.
Qed.


(********************************************************************)
(* Lifting after substiution, 
   with the ligting at a higher index *)
Lemma liftTT_substTT'
 :  forall n n' t1 t2
 ,  liftTT 1 (n + n') (substTT n t2 t1)
 =  substTT n (liftTT 1 (n + n') t2) (liftTT 1 (1 + n + n') t1).
Proof.
 intros. gen n n' t2.
 induction t1; intros; try (repeat rewritess); eauto.

 Case "TVar".
  repeat ( unfold liftTT; unfold substTT; fold liftTT; fold substTT
         ; try lift_cases
         ; try fbreak_nat_compare
         ; intros); try burn.

 Case "TForall".
  simpl. rewrite (IHt1 (S n) n').
  simpl. rewrite (liftTT_liftTT_11 0 (n + n')). auto.
Qed.


(********************************************************************)
(* Commuting substitutions. *)
Lemma substTT_substTT
 :  forall n m t1 t2 t3
 ,  substTT (n + m) t3 (substTT n t2 t1)
 =  substTT n (substTT (n + m) t3 t2)
              (substTT (1 + n + m) (liftTT 1 n t3) t1).
Proof.
 intros. gen n m t2 t3.
 induction t1; intros; eauto.

 Case "TVar".
  repeat (simpl; fbreak_nat_compare); try burn.
  rewrite substTT_liftTT. auto.

 Case "TForall".
  simpl. f_equal.
  rewrite (IHt1 (S n) m). 
  f_equal; simpl.
   rewrite (liftTT_substTT_1 0 (n + m)). auto.
   rewrite (liftTT_liftTT_11 0 n). auto.  

 Case "TApp".
  simpl. f_equal.
   apply IHt1_1.
   apply IHt1_2.
Qed.


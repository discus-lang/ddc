
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
(* Substitution and type utils *)
Lemma substTT_getCtorOfType
 :  forall d t1 t2 t3
 ,  getCtorOfType t2                = Some t3
 -> getCtorOfType (substTT d t1 t2) = Some t3.
Proof.
 intros. gen d t1 t3.
 induction t2; try burn.
Qed.  
Hint Resolve substTT_getCtorOfType.


Lemma substTT_makeTApps 
 :  forall d t2 t1 ts
 ,  substTT d t2 (makeTApps t1 ts) 
 =  makeTApps (substTT d t2 t1) (map (substTT d t2) ts).
Proof.
 intros. gen d t2 t1.
 induction ts; try burn.
Qed.
Hint Rewrite substTT_makeTApps : global.


Lemma liftTT_makeTApps
 :  forall d n t1 ts
 ,  liftTT n d (makeTApps t1 ts)
 =  makeTApps (liftTT n d t1) (map (liftTT n d) ts).
Proof.
 intros. gen d n t1.
 induction ts; burn.
Qed.
Hint Rewrite liftTT_makeTApps : global.


Lemma takeTCon_substTT
 :  forall ix t1 tc t2
 ,  takeTCon t1                 = TCon tc
 -> takeTCon (substTT ix t2 t1) = TCon tc.
Proof.
 intros. gen ix t2.
 induction t1; burn.
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
   fbreak_nat_compare; burn. 
Qed.
Hint Rewrite substTT_liftTT : global.


(********************************************************************)
(* Lifting after substitution,
   with the lifting at a lower index. *)
Lemma liftTT_substTT_1
 :  forall n n' t1 t2
 ,  liftTT 1 n (substTT (n + n') t2 t1)
 =  substTT (1 + n + n') (liftTT 1 n t2) (liftTT 1 n t1).
Proof.
 intros. gen n n' t2.
 induction t1; intros; simpl; try burn.

 Case "TVar".
  repeat (simpl; fbreak_nat_compare; 
          try lift_cases; try intros); burn.

 Case "TForall".
  rewrite (IHt1 (S n) n').
  rewrite (liftTT_liftTT_11 0 n).
  burn.
Qed.


Lemma liftTT_substTT
 :  forall m n n' t1 t2
 ,  liftTT m n (substTT (n + n') t2 t1)
 =  substTT (m + n + n') (liftTT m n t2) (liftTT m n t1).
Proof.
 intros. gen n n'.
 induction m; intros; simpl.
  burn.

  rrwrite (S m = 1 + m).
  rewrite <- liftTT_plus.
  rewrite IHm.
  rrwrite (m + n + n' = n + (m + n')).
  rewrite liftTT_substTT_1. 
  burn.
Qed.
Hint Rewrite <- liftTT_substTT : global.


(********************************************************************)
(* Lifting after substiution, 
   with the ligting at a higher index *)
Lemma liftTT_substTT'
 :  forall n n' t1 t2
 ,  liftTT 1 (n + n') (substTT n t2 t1)
 =  substTT n (liftTT 1 (n + n') t2) (liftTT 1 (1 + n + n') t1).
Proof.
 intros. gen n n' t2.
 induction t1; intros; try burn.

 Case "TVar".
  repeat ( unfold liftTT; unfold substTT; fold liftTT; fold substTT
         ; try lift_cases
         ; try fbreak_nat_compare
         ; intros); burn.

 Case "TForall".
  simpl. rewrite (IHt1 (S n) n').
  simpl. rewrite (liftTT_liftTT_11 0 (n + n')). 
  burn.
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
 induction t1; intros; try burn.

 Case "TVar".
  repeat (simpl; fbreak_nat_compare); burn.

 Case "TForall".
  simpl. f_equal.
  rewrite (IHt1 (S n) m). 
  f_equal.
   rewrite (liftTT_substTT_1 0 (n + m)). auto.
   rewrite (liftTT_liftTT_11 0 n). auto.  
Qed.


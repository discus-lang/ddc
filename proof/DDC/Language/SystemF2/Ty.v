
Require Export DDC.Language.SystemF2.Ki.


(* Type Constructors. *)
Inductive tycon : Type :=
 | TyConFun  : tycon
 | TyConData : nat   -> ki -> tycon.
Hint Constructors tycon.


(* Type Expressions. *)
Inductive ty  : Type :=
 | TCon      : tycon -> ty
 | TVar      : nat   -> ty
 | TForall   : ty    -> ty
 | TApp      : ty    -> ty -> ty.
Hint Constructors ty.


(* Baked in types. *)
Definition tFun (t1: ty) (t2: ty)
 := TApp (TApp (TCon TyConFun) t1) t2.
Hint Unfold tFun.


(* Well formed types are closed under the given kind environment. *)
Inductive wfT (ke: kienv) : ty -> Prop :=
 | WfT_TVar 
   :  forall i
   ,  (exists k, get i ke = Some k)
   -> wfT ke (TVar i)

 | WfT_TCon
   :  forall n
   ,  wfT ke (TCon n)

 | WfT_TForall
   :  forall t
   ,  wfT (ke :> KStar) t
   -> wfT ke (TForall t)

 | WfT_TApp
   :  forall t1 t2
   ,  wfT ke t1 -> wfT ke t2
   -> wfT ke (TApp t1 t2).
Hint Constructors wfT.


(* Closed types are well formed under an empty environment. *)
Definition closedT (tt: ty) : Prop
 := wfT nil tt.
Hint Unfold closedT.


(*******************************************************************)
(* Lifting *)
(* Lift type indices that are at least a certain depth. *)
Fixpoint liftTT (d: nat) (tt: ty) : ty :=
  match tt with
  |  TVar ix
  => if le_gt_dec d ix
      then TVar (S ix)
      else tt

  |  TCon _     => tt

  |  TForall t 
  => TForall (liftTT (S d) t)

  |  TApp t1 t2
  => TApp    (liftTT d t1) (liftTT d t2)
  end.
Hint Unfold liftTT.


(* Tactic to help deal with lifting functions *)
Ltac lift_cases 
 := match goal with 
     |  [ |- context [le_gt_dec ?n ?n'] ]
     => case (le_gt_dec n n')
    end.


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
    => TForall (substTT (S d) (liftTT 0 u) t)

    |  TApp t1 t2 
    => TApp (substTT d u t1) (substTT d u t2)
  end.


(********************************************************************)
(* Changing the order of lifting. *)
Lemma liftTT_liftTT
 :  forall n n' t
 ,  liftTT n              (liftTT (n + n') t) 
 =  liftTT (1 + (n + n')) (liftTT n t).
Proof.
 intros. gen n n'.
 induction t; intros; auto.

 Case "TVar".
  simpl.
  repeat (unfold liftTT; lift_cases; intros); burn.

 Case "TForall".
  simpl.
  assert (S (n + n') = (S n) + n'). omega. rewrite H. 
  rewrite IHt. auto.

 Case "TApp".
  simpl. apply f_equal2; auto.
Qed.  


(* If we lift at depth d, this creates an empty space and
   substituting into it doens't do anything. *)
Lemma substTT_liftTT
 :  forall d t1 t2
 ,  substTT d t2 (liftTT d t1) = t1.
Proof.
 intros. gen d t2.
 induction t1; intros; eauto.

 Case "TVar".
  simpl; lift_cases; unfold substTT;
   fbreak_nat_compare; intros;
   burn.

 Case "TForall".
  simpl. 
  rewrite IHt1. auto.

 Case "TApp".
  simpl.
  rewrite IHt1_1.
  rewrite IHt1_2. auto.
Qed.


(* Lifting after substitution *)
Lemma liftTT_substTT
 :  forall n n' t1 t2
 ,  liftTT n (substTT (n + n') t2 t1)
 =  substTT (1 + n + n') (liftTT n t2) (liftTT n t1).
Proof.
 intros. gen n n' t2.
 induction t1; intros; eauto.

 Case "TVar".
  repeat (simpl; fbreak_nat_compare; 
          try lift_cases; try intros);
   burn.

 Case "TForall".
  simpl.
  rewrite (IHt1 (S n) n'). simpl.
  rewrite (liftTT_liftTT 0 n). auto.

 Case "TApp".
  simpl.
  rewrite IHt1_1. auto.
  rewrite IHt1_2. auto.
Qed.


Lemma liftTT_substTT'
 :  forall n n' t1 t2
 ,  liftTT (n + n') (substTT n t2 t1)
 =  substTT n (liftTT (n + n') t2) (liftTT (1 + n + n') t1).
Proof.
 intros. gen n n' t2.
 induction t1; intros; auto.

 Case "TVar".
  repeat ( unfold liftTT; unfold substTT; fold liftTT; fold substTT
         ; try lift_cases; try fbreak_nat_compare
         ; intros); burn.

 Case "TForall".
  simpl. f_equal.
  rewrite (IHt1 (S n) n'). f_equal.
   simpl. rewrite (liftTT_liftTT 0 (n + n')). auto.

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
              (substTT (1 + n + m) (liftTT n t3) t1).
Proof.
 intros. gen n m t2 t3.
 induction t1; intros; auto.

 Case "TVar".
  repeat (simpl; fbreak_nat_compare); try burn.
  rewrite substTT_liftTT. auto.

 Case "TForall".
  simpl. f_equal.
  rewrite (IHt1 (S n) m). f_equal.
   simpl. rewrite (liftTT_substTT 0 (n + m)). auto.
   simpl. rewrite (liftTT_liftTT 0 n). auto.  

 Case "TApp".
  simpl. f_equal.
   apply IHt1_1.
   apply IHt1_2.
Qed.


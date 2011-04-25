
Require Import TyJudge.
Require Export Exp.


(** Substitution ****************************************************)

(* When we push new elements on the environment stack of an
   expression, we need to lift free indices in the expression 
   across the new elements.

   For example given: 
             t1, t0 |- 0 1 (\. 0 1 2) :: t3

   Pushing two more elements gives:
     t1, t0, ta, tb |- 2 3 (\. 0 3 4) :: t3
 *)
Fixpoint 
 liftX  (n:  nat) (* number of new elements pushed on environment *)
        (d:  nat) (* current binding depth in expression *)
        (xx: exp) (* expression to lift *)
        : exp
 := match xx with 
    |  XVar ix    
    => if bge_nat ix d
        (* var was pointing into env, lift it across new elems *)
        then XVar (ix + n)
        (* var was locally bound, leave it be *)
        else xx

    (* increase the depth as we move across a lambda *)
    |  XLam t1 x1
    => XLam t1 (liftX n (S d) x1)

    |  XApp x1 x2
    => XApp   (liftX n d x1) (liftX n d x2)
    end.


(* Substitute for the outermost binder in an expression. *)
Fixpoint
 subst' (d:  nat) (* current binding depth in expression *)
        (u:  exp) (* new expression to substitute *)
        (xx: exp) (* expression to substitute into *)
        : exp 
 := match xx with
    | XVar ix 
    => match compare ix d with
       (* Index matches the one we are substituting for. 
          When we substitute the new expression, we need to lift
          its free indices across the lambdas that we've crossed
          to get to this point *)
       | EQ => liftX d 0 u
       
       (* Index was free in the original expression.
          As we've removed the outermost binder, also decrease this
          index by one. *)
       | GT => XVar (ix - 1)

       (* Index was bound in the original expression. *)
       | LT  => XVar ix
       end

    (* increase the depth as we move across a lambda *)
    |  XLam t1 x2
    => XLam t1 (subst' (S d) u x2)

    |  XApp x1 x2 
    => XApp (subst' d u x1) (subst' d u x2)
 end. 


Definition  subst := subst' 0.
Hint Unfold subst.


(** Lemmas **********************************************************)

(* Lifting an expression by 0 steps doesn't do anything.
   This is equivalent to pushing 0 things on the environment. *)
Theorem liftX_none
 : forall x1 depth
 , liftX 0 depth x1 = x1.
Proof.
 induction x1; intro; simpl.

 Case "XVar".
  assert (n + 0 = n). omega. rewrite H.
  breaka (bge_nat n depth).

 Case "XLam".
  rewrite IHx1. auto.

 Case "XApp". 
  rewrite IHx1_1. rewrite IHx1_2. auto.
Qed.


(* If an expression is well formed under a given environment, 
   then all its indices are less than the length of this environment. 
   Lifting indices more than this length doesn't do anything *)
Theorem liftX_wfX
 :  forall n d x e
 ,  d = length e
 -> wfX e x
 -> liftX n d x = x.
Proof.
 intros. gen e d.
 induction x; intros; simpl; simpl in H0.
 
 Case "XVar".
  breaka (bge_nat n0 d).
  apply bge_nat_true in HeqX.
  false. subst. destruct H0.
  eapply get_above_false in H; auto.

 Case "XLam".
  eapply IHx in H0; eauto.
  simpl in H0. symmetry. rewrite H. rewrite H0. auto.

 Case "XApp".
  destruct H0.
  lets D1: IHx1 H0 H. rewrite D1.
  lets D2: IHx2 H1 H. rewrite D2.
  auto.
Qed.


(* If an expression is closed then it has no free indices. 
   Lifting it doesn't do anything. *)
Theorem liftX_closed
 :  forall n x
 ,  closedX x
 -> liftX n 0 x = x.
Proof.
 intros. unfold closedX in H. eapply liftX_wfX; eauto. 
 simpl. auto.
Qed.


(* Substitution of values in values. 
   Inductively, we need to reason about performing substitutions 
   at any depth, hence we must prove a property about (subst' d x2 x1) 
   instead of the weaker (subst x2 x1) that assumes the substitution
   is taking place at top level.
 *)
Theorem subst_value_value_drop
 :  forall ix e x1 x2 t1 t2
 ,  get  e ix = Some t2
 -> TYPE e           x1 t1
 -> TYPE (drop ix e) x2 t2
 -> TYPE (drop ix e) (subst' ix x2 x1) t1.
Proof.
 intros ix e x1 x2 t1 t2. gen ix e t1.
 induction x1; intros; simpl; inverts H0.

 Case "XVar".
  fbreak_compare.
  SCase "i = ix".
   rewrite H in H4. inverts H4.
   lets D: type_wfX H1.

  SCase "n < ix".
   apply TYVar. rewrite <- H4. auto.

  SCase "n > ix".
   apply TYVar. rewrite <- H4.
   destruct n.
    false. omega.
    simpl. rewrite nat_minus_zero. apply get_drop_below. omega.

 Case "XLam".
  apply TYLam. rewrite drop_rewind.
  apply IHx1; auto.
  eapply type_check_closed_in_any_tyenv; eauto. admit.

 Case "XApp".
  eauto.
Qed.


Theorem subst_value_value
 :  forall tenv x1 x2 t1 t2
 ,  closedX x2
 -> TYPE (tenv :> t2) x1 t1
 -> TYPE tenv         x2 t2 
 -> TYPE tenv (subst x2 x1) t1.
Proof. 
 intros tenv x1 x2 t1 t2 Ht1 Ht2.
 lets H: subst_value_value_drop 0 (tenv :> t2).
  simpl in H. eapply H; eauto.
Qed.


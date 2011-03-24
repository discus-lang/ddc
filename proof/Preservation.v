
Require Import TyJudge.
Require Import EvJudge.


Theorem preservation
 :  forall env t t' T
 ,  TYPE env t  T
 -> STEP t t'
 -> TYPE env t' T.
Proof.
 intros env t t' T Htype Hstep.
 generalize dependent t'.
 induction t.
 Case "XVar". 
  intros. inversion Hstep.
 Case "XLam".
  intros. inversion Hstep.
 Case "XApp".
  intros. inversion Hstep. subst.
  inversion H0. subst.
  SCase "EVAppAbs".
   inversion H4. subst.
   eapply subst_value_value.
   apply values_are_closed in H5.
   unfold closed in H5.
   intro. specialize H5 with z.
   intro. tauto.
   apply H3. assumption.
  SCase "EVApp1". subst.
   apply IHt1.
   subst. eapply TYApp.
   
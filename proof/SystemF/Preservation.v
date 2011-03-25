
Require Import TyJudge.
Require Import EvJudge.


(* When a well typed term transitions to the next state, 
   its type is perserved.
 *)
Theorem preservation
 :  forall env t t' T
 ,  TYPE env t  T
 -> STEP t t'
 -> TYPE env t' T.
Proof.
 intros env t t' T Htype Hstep.
 generalize dependent t'.
 generalize dependent T.
 induction t.

 Case "XVar". 
  intros. inversion Hstep.

 Case "XLam".
  intros. inversion Hstep.

 Case "XApp".
  intros. inversion Hstep. subst.
  SCase "EVAppAbs".
   inversion Htype. subst.
   eapply subst_value_value.
    intro. 
     apply values_are_closed in H2.
     unfold closed in H2.
     specialize H2 with z. intro. contradiction.
    inversion H3. eauto.
    auto.
  SCase "EVApp1". subst.
   inversion Htype. subst. eauto. 
  SCase "EVApp2". subst.
   inversion Htype. subst. eauto.
Qed.   

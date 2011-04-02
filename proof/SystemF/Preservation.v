
Require Import TyJudge.
Require Import EvJudge.
Require Import SubValueValue.
Require Import SubTypeValue.


(* When a well typed term transitions to the next state, 
   its type is perserved.
 *)
Theorem preservation
 :  forall kenv tenv t t' T
 ,  TYPE kenv tenv t  T
 -> STEP t t'
 -> TYPE kenv tenv t' T.
Proof.
 intros kenv tenv t t' T Htype Hstep.
 gen t' T.
 induction t; intros.

 Case "XVar". 
  inversions Hstep.

 Case "XLam".
  inversion Hstep.

 Case "XApp".
  inversions Hstep.
  SCase "EVAppAbs".
   inversions Htype.
   eapply subst_value_value.
    intro. 
     apply values_are_closed in H2.
     unfold closedX in H2.
     specialize H2 with z. intro. contradiction.
    inversion H4. eauto.
    auto.
  SCase "EVApp1".
   inversions Htype. eauto.
  SCase "EVApp2".
   inversions Htype. eauto.

 Case "XLAM".
  inversions Hstep.

 Case "XAPP".
  inverts keep Hstep.
  SCase "EVAPPLAM".
   inversions Htype.
   inversions H3.
    (* need that T2 is closed add to EVAPPABS *)
   

Qed.   


Require Import TyJudge.
Require Import EvJudge.
Require Import SubstValueValue.


(* When a well typed term transitions to the next state, 
   its type is perserved. *)
Theorem preservation
 :  forall x x' t
 ,  TYPE Empty x  t
 -> STEP x x'
 -> TYPE Empty x' t.
Proof.
 intros x x' t HT HS. gen t x'.
 induction x; intros.

 (* These can't happen as there is no evaluation rule *)
 Case "XVar". inversion HS.
 Case "XLam". inversion HS.

 (* Applications *)
 Case "XApp".
  inversions HT.
  inverts keep HS.

  SCase "EVLamApp".
   inversions H2.
   inversions H3.
   eapply subst_value_value; eauto.

  SCase "EVApp1".
   eapply IHx1 in H2; eauto.
   eapply IHx2 in H4; eauto.
Qed.



Require Import TyJudge.
Require Import EvJudge.
Require Import SubstValueValue.


(* When a well typed term transitions to the next state, 
   its type is perserved. *)
Theorem preservation
 :  forall t t' T
 ,  TYPE Empty t  T
 -> STEP t t'
 -> TYPE Empty t' T.
Proof.
 intros t t' T HT HS. gen T t'.
 induction t; intros.

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
   eapply IHt1 in H2; eauto.
   eapply IHt2 in H4; eauto.
Qed.


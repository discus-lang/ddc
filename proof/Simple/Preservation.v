
Require Import TyJudge.
Require Import EsJudge.
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
 Case "XVar". invert HS.
 Case "XLam". invert HS.

 (* Applications *)
 Case "XApp".
  inverts HT.
  inverts keep HS.

  SCase "EVLamApp".
   inverts H2.
   inverts H3.
   eapply subst_value_value; eauto.

  SCase "EVApp1".
   eapply IHx1 in H2; eauto.
   eapply IHx2 in H4; eauto.
Qed.


(* When we multi-step evaluate some expression,
   then the result has the same type as the original.
 *)  
Lemma preservation_steps
 :  forall x1 t1 x2
 ,  TYPE Empty x1 t1
 -> STEPS      x1 x2
 -> TYPE Empty x2 t1.
Proof.
 intros. 
 induction H0; eauto.
  eapply preservation; eauto.
Qed.



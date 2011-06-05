
Require Import TyJudge.
Require Import EsJudge.
Require Import SubstExpExp.


(* When a well typed expression transitions to the next state
   then its type is preserved.
 *)
Theorem preservation
 :  forall ds x x' t
 ,  TYPE ds Empty x  t
 -> STEP x x'
 -> TYPE ds Empty x' t.
Proof.
 intros ds x x' t HT HS. gen t.
 induction HS; intros.

 (* Evaluation in an arbitrary context. *)
 Case "EsContext".
  destruct H; eauto; 
   try (inverts HT; progress eauto).

  (* The constructor case is tricky because we need to handle
     an aribrary number of constructor arguments. *)
  SCase "XCon".
   inverts HT.
   eapply TYCon. eauto.

   clear H4.    (* clear so it's doesn't get in the way of induction *)
   gen tsArgs.  (* We need the IH to work for an arbitraty length
                   of remaining arguments. *)
   induction H. (* induction over length of context *)
    intros. inverts H6. eauto.
    intros. inverts H6. eauto.

 Case "EsLamApp".
  inverts HT. inverts H4.
  eapply subst_value_value; eauto.

 Case "EsCaseAlt".
  inverts HT. inverts H5.
  (* TODO: finish this. need subst for list of exps *)
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


(* When we multi-step evaluate some expression, 
   then the result has the same type as the original.
   Using the left-linearised form for the evaluation.
 *)
Lemma preservation_stepsl
 :  forall x1 t1 x2
 ,  TYPE Empty x1 t1
 -> STEPSL x1 x2
 -> TYPE Empty x2 t1.
Proof.
 intros. 
 induction H0.
  auto.
  apply IHSTEPSL.
  eapply preservation. 
   eauto. auto.
Qed.



Require Import DDC.Language.SimpleData.StepContext.
Require Import DDC.Language.SimpleData.Step.
Require Import DDC.Language.SimpleData.Exp.
Require Import DDC.Base.


(* Chain of evaluation contexts. 
   This carries the evaluation contexts needed to reduce
   a list of expressions in left-to-right order.
 
   Given a list of expressions:
     xs = [x1 x2 x3 x4 x5 x6 x7 x8]

   We want to prove that if each one is either a value or can be reduced
   to one, then we can evaluate the whole list of expressions to a list
   of values.
     vs = [v1 v2 v3 v4 v5 v6 v7 v8]

   To do this we need a list of evaluation contexts like follows, 
   where XX is the expression currently being evaluated.
     C0:  [XX x1 x2 x3 x4 x5 x6 x7]
     C1:  [v0 XX x2 x3 x4 x5 x6 x7]
     C2:  [v0 v1 XX x3 x4 x5 x6 x7]
     C3:  [v0 v1 v2 XX x4 x5 x6 x7]
           ...
     C7:  [v0 v1 v2 v3 v4 v5 v6 XX ]     

   The proof using these contexts proceeds *backwards* over 
   the number of expressions that are known to be values.

   1) If all expressions are values then we're already done.
   2) For some context C, if we can reduce (C v) to all values,
      and we can reduce x to v, then we can also reduce (C x) to 
      all values. This reasoning is similar to that used by the
      eval_expansion lemma.
   3) Producing the base-level context (C0) is trivial. We don't
      need to show that expressions to the left are already values,
      because there aren't any.
*)
Inductive CHAIN : list exp -> list exp -> Prop :=
 | EcDone
   :  forall vs
   ,  Forall wnfX vs
   -> CHAIN vs vs

 | EcCons
   :  forall x v vs C
   ,  exps_ctx C  
   -> STEPS x v -> wnfX v
   -> CHAIN (C v) vs
   -> CHAIN (C x) vs.

Hint Constructors CHAIN.


Lemma chain_extend
 :  forall v xs ys
 ,  wnfX v 
 -> CHAIN xs ys
 -> CHAIN (v :: xs) (v :: ys).
Proof.
 intros.
 induction H0.
  auto.
  lets D1: XscCons v H0. auto.
  lets D2: EcCons D1 H1 H2 IHCHAIN. 
  auto.
Qed.


Lemma make_chain
 :  forall xs vs
 ,  Forall2 STEPS xs vs
 -> Forall  wnfX vs
 -> CHAIN xs vs.
Proof.
 intros. gen vs.
  induction xs as [xs | x]; intros.
  inverts H. auto.
 
  destruct vs as [vs | v].
   inverts H.

  inverts H. inverts H0.
  assert (CHAIN xs vs). auto.
   clear IHxs.

  (* TODO: this comes from STEPS xs vs *)
  assert (Forall2 (fun x v => STEPS x v /\ wnfX v /\ (wnfX x -> v = x)) xs vs).
   eapply (@Forall2_impl_in exp exp STEPS). intros.
   split. auto.
   split. rewrite Forall_forall in H3. auto.
   intros.
   apply steps_wnfX. auto. auto. auto.
   

  (* either all the xs are already whnfX,
      or there is a context where one can step *)
  lets D: exps_ctx2_run H0. clear H0.
  inverts D.

  Case "all whnfX".
    assert (Forall2 eq xs vs).
    eapply (@Forall2_impl_in exp exp STEPS (@eq exp) xs vs).
     intros. 
      symmetry.
       apply steps_wnfX. 
       rewrite Forall_forall in H0. auto.
       auto.
       auto.
    assert (xs = vs).
     apply Forall2_eq. auto. subst.
   
   lets C1: XscHead vs.
   lets D1: EcCons x v (v :: vs) C1 H4.
    apply D1. auto.
    auto.

  Case "something steps".
   destruct H0 as [C1].
   destruct H0 as [C2].
   destruct H0 as [x'].
   destruct H0 as [v'].
   inverts H0. inverts H5. inverts H7.

   lets D1: exps_ctx2_left H0.
   lets D2: exps_ctx2_right H0.

   assert (wnfX v').
    eapply exps_ctx_Forall. 
    eapply D2. auto.

   lets E1: XscCons C1 H2 D1.
   lets E2: XscCons C2 H2 D2.

   lets E3: XscHead (C1 x').

   lets F1: EcCons x v (v :: C2 v') E3.
    apply F1. auto. auto. clear F1.

   eapply chain_extend. auto. auto.
Qed.


Lemma steps_chain_XCon
 :  forall xs vs dc
 ,  CHAIN xs vs
 -> STEPS (XCon dc xs) (XCon dc vs).
Proof.
 intros.
 induction H; auto.
  eapply (EsAppend (XCon dc (C x)) (XCon dc (C v))); auto.
  eapply steps_context_XCon; auto.
Qed.


Lemma steps_in_XCon
 :  forall xs vs dc
 ,  Forall2 STEPS xs vs
 -> Forall wnfX vs
 -> STEPS (XCon dc xs) (XCon dc vs).
Proof.
 intros xs vs dc HS HW.
 lets HC: make_chain HS HW.
 eapply steps_chain_XCon. auto.
Qed.


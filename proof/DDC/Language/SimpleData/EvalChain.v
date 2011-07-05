
Require Import DDC.Language.SimpleData.StepContext.
Require Import DDC.Language.SimpleData.Step.
Require Import DDC.Language.SimpleData.Exp.
Require Import DDC.Base.


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
  lets D: exps_ctx_run_Forall2' H0. clear H0.
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
 induction H.
  eauto.
  eapply (EsAppend (XCon dc (C x)) (XCon dc (C v))).
   eapply steps_context_XCon.
     auto. auto.
   auto.
Qed.

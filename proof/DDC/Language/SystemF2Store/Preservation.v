
Require Import DDC.Language.SystemF2Store.Step.
Require Import DDC.Language.SystemF2Store.TyJudge.
Require Import DDC.Language.SystemF2Store.SubstExpExp.
Require Import DDC.Language.SystemF2Store.SubstTypeExp.

Lemma replace_nil
 : forall {A} n (x : A)
 , replace n x nil = nil.
Proof.
 destruct n; burn.
Qed.

Lemma replace_length
 : forall {A} n x (xs : list A)
 , length (replace n x xs) = length xs.
Proof.
 intros. gen n.
 induction xs; intros.
  rewrite replace_nil; auto.
  destruct n; burn.
Qed. 


(* When a well typed expression transitions to the next state
   then its type is preserved. *)
Theorem preservation
 :  forall ds se s s' x x' t
 ,  WfS ds se s
 -> TYPE ds nil nil se x  t
 -> STEP s x s' x'
 -> (exists se', extends se' se
             /\  WfS ds  se' s'
             /\  TYPE ds nil nil se' x' t).
Proof.
 intros ds se s s' x x' t HW HT HS. gen t.
 induction HS; intros; inverts_type; eauto.

 (* Evaluation in an arbitrary context. *)
 Case "EsContext".
  spec IHHS HW.
  destruct H; try 
   (inverts_type; 
    edestruct IHHS as [se2]; eauto; 
    exists se2; int; eauto).

  SCase "XCon".
   inverts_type.
   assert (exists t, TYPE ds nil nil se x t) as HX.
    eapply (@exps_ctx_Forall2_exists_left exp ty wnfX C); eauto.
   dest t. 
   edestruct IHHS as [se2]; eauto.
   exists se2; int.

   eapply TyCon; eauto.
   assert (Forall2 (TYPE ds nil nil se2) (C x) (map (substTTs 0 ts) tsFields)) as HF.
    eapply Forall2_impl with (R1 := TYPE ds nil nil se). eauto. eauto.

    admit. (* fark. Forall2 lemma *)

  SCase "XCase".
   eapply TyCase; eauto. 
   nforall. intros.
   apply H2 in H5.
   admit. (* ok, need store typing weakening for TYPEA *)

 Case "EsLamApp".
  exists se. int.
  eapply subst_exp_exp; eauto.

 Case "EsLAMAPP".
  exists se. int.
  assert (TYPE ds nil (substTE 0 t2 nil) (substTE 0 t2 se)
                      (substTX 0 t2 x12) (substTT 0 t2 t1)) as HT.
   eapply subst_type_exp; eauto.
   have (Forall closedT se).
   assert (liftTE 0 se = se) as HL.
    admit. (* ok se all closed *)
   rewrite HL in H2.
   rrwrite (liftTE 0 nil = nil) in H2. auto.
  rrwrite (substTE 0 t2 nil = nil) in HT.

   assert (substTE 0 t2 se  = se) as HS.
    admit. (* ok se all closed *)
   rewrite HS in HT.
  auto.

 Case "EsAlloc".
  exists ((makeTApps (TCon tc) tsParam) <: se).
  int. 
  admit. (* TODO: show extended store still well formed *)
  eapply TyLoc.
  admit. (* ok get lemma *)
  skip.  (* TODO: fixme *)
  defok ds (DefDataType tc ks dcs). eauto.

 Case "EsCaseAlt".
  skip.
(* exists se. int.
   eapply subst_exp_exp_list.
   skip. (* ok svs are wf *)

   have (In (AAlt dc x) alts).
   nforall.

  have (TYPEA ds nil nil (AAlt dc x) (makeTApps (TCon tc) ts) t) as HA.
  inverts HA.
  rewrite H11 in H16. inverts H16.
  rewrite H15 in H10. inverts H10.

  have (getCtorOfType (TCon tc0) = Some tc0) as HTC.
   erewrite getCtorOfType_makeTApps in H5; eauto.
   inverts H5.
  rewrite H6 in H15. inverts H15.
  rr.
  have (length ts = length ks0)      as HTK1.
  have (length tsParam = length ks0) as HTK2.
  rewrite <- HTK1 in HTK2.
  assert (tsParam = ts).
   eapply makeTApps_args_eq; eauto. 
   subst.
  eauto.
 *)

 Case "EsUpdate".
  symmetry in H0. subst.
  exists se. rip.
  inverts keep HW.
  unfold WfS.
   rip; eauto.
    unfold STOREM in *.
     rs. rewrite replace_length. auto.

    (* We can get the old field value that is being replaced. *)
    assert (exists svField vField
               ,  get i svs = Some svField
               /\ svalueOf vField svField) as HL.
     eapply storet_field_has; eauto. admit.                (* fine, list lemma *)
     destruct HL  as [svField0].
     destruct H10 as [vField0].
     rip. 

    (* The old field value has the same type as the one we're replacing it with *)
    assert (TYPE ds nil nil se vField0 (substTTs 0 tsParam tField)) as HF.
     eapply storet_field_type; eauto.

    (* When we replace the field the store is still well typed. *)
    eapply storet_replace_field
      with (vField2 := vField); eauto.
    
    admit. (* TODO: need type def for unit *)

 Case "EsUpdateSkip".
  exists se. int.
  unfold xUnit. unfold tUnit.
  rrwrite ( TCon (TyConData 0 KStar)
          = makeTApps (TCon (TyConData 0 KStar)) nil).
  eapply TyCon with (tsFields := nil) (dcs := nil); eauto.
  skip. skip. (* ok, need to bake in the DataDefs for unit *)
  rrwrite (map (substTTs 0 nil) (@nil ty) = (@nil ty)).
  auto.
Qed.



(* When we multi-step evaluate some expression, 
   then the result has the same type as the original.
   Using the left-linearised form for the evaluation.
 *)
Lemma preservation_stepsl
 :  forall ds x1 t1 x2 se s s'
 ,  WfS  ds se s
 -> TYPE ds nil nil se  x1 t1
 -> STEPSL      s   x1 s' x2
 -> (exists se', extends se' se
              /\ WfS  ds se' s'
              /\ TYPE ds nil nil se' x2 t1).
Proof.
 intros ds x1 t1 x2 se s s' HW HT HS. gen se.
 induction HS; intros.
  Case "EslNone".
   eauto.
  Case "EslCons".
   lets D: preservation HW HT H.
    destruct D as [se2].
    int.
   spec IHHS H0 H3.
   destruct IHHS as [se3].
   int.
   exists se3. int.
   eapply extends_trans; eauto.
Qed.


(* When we multi-step evaluate some expression,
   then the result has the same type as the original. *)  
Lemma preservation_steps
 :  forall ds x1 t1 x2 se s s'
 ,  WfS ds se s
 -> TYPE ds nil nil se x1 t1
 -> STEPS        s  x1 s' x2
 -> (exists se', extends se' se
              /\  WfS ds  se' s'
              /\  TYPE ds nil nil se' x2 t1).
Proof.
 intros ds x1 t1 x2 se s s' HW HT HS.
 eapply stepsl_of_steps in HS.
 eapply preservation_stepsl; eauto.
Qed.


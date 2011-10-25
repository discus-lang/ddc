
Require Import DDC.Language.SystemF2Store.Step.
Require Import DDC.Language.SystemF2Store.TyJudge.
Require Import DDC.Language.SystemF2Store.SubstExpExp.
Require Import DDC.Language.SystemF2Store.SubstTypeExp.


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


 (** Context *******************************)
 Case "EsContext".
  rip.
  destruct H; try 
   (inverts_type; 
    edestruct IHHS as [se2]; eauto; 
    exists se2; rip; eauto).

  SCase "XCon".
   inverts_type.
   have (exists t, TYPE ds nil nil se x t) as HX
     by (eapply (@exps_ctx_Forall2_exists_left exp ty wnfX C); eauto).
   dest t.

   spec IHHS H0.
   destruct IHHS as [se2].
   exists se2; rip.
  
   eapply TyCon; eauto.
   eapply (exps_ctx_Forall2_swap wnfX (TYPE ds nil nil se2) C x); auto.
    intros.

   have (y = t)
    by (eapply type_unique; eauto).
   subst.

   eauto.

   have  (Forall2 (TYPE ds nil nil se2) (C x) (map (substTTs 0 ts) tsFields)) as HF
    by   (eapply Forall2_impl with (R1 := TYPE ds nil nil se); eauto; eauto).
   auto.

  SCase "XCase".
   eapply TyCase; eauto. 
   nforall. intros.
   apply H2 in H5.
   eauto.


 (** LamApp *****************************)
 Case "EsLamApp".
  exists se. rip.
  eapply subst_exp_exp; eauto.


 (* LAMAPP *****************************)
 Case "EsLAMAPP".
  exists se. rip.
  assert (TYPE ds nil (substTE 0 t2 nil) (substTE 0 t2 se)
                      (substTX 0 t2 x12) (substTT 0 t2 t1)) as HT.
   eapply subst_type_exp; eauto.
   have (Forall closedT se).
   rw (liftTE  0 se     = se)  in H2.
   rw (liftTE  0 nil    = nil) in H2. auto.
   rw (substTE 0 t2 nil = nil) in HT.
   rw (substTE 0 t2 se  = se)  in HT. auto.
   auto.


 (* Alloc *****************************)
 Case "EsAlloc".
  exists ((makeTApps (TCon tc) tsParam) <: se). rip.

  (* Store extended with the new binding is well formed *)
  eapply store_extended_wellformed; eauto.

  (* New store location is well typed under the store typing. *)
  eapply TyLoc with (tc := tc); eauto.
   have (length s = length se) as HL
    by  (unfold WfS in *; burn).
   rewrite HL. eauto.
   have (DEFOK ds (DefType tc ks dcs)) as HD.
   inverts HD.
   eauto.


 (** Case *****************************)
 Case "EsCaseAlt".
  exists se.  rip.
  inverts HW. rip.
  unfold STORET in *.
  spec H14 H.
  destruct H14 as [tcObj'].
  destruct H13 as [tsParam'].
  destruct H13 as [tsFields']. 
  rip.

  rewrite H14 in H8. inverts H8.
  have (tcObj = tc) by congruence. subst.

  assert (tcObj' = tc).
   rewrite (getCtorOfType_makeTApps tcObj') in H6; auto.
   inverts H6. auto.
  subst.

  have (In dc dcs).
  have (In (AAlt dc x) alts).

  have (TYPEA ds nil nil se (AAlt dc x) (makeTApps (TCon tc) tsParam') t) as HA
   by  (nforall; auto).

  have (vs = map expOfSValue svs).
   subst.

  eapply subst_exp_exp_list. eauto.
  inverts HA. defs_merge. 

  assert (tsParam' = tsParam).
   apply makeTApps_eq_params in H19. rip.
   subst. 
  auto.
  

 (** Update ***************************)
 Case "EsUpdate".
  symmetry in H0. subst.
  exists se. rip.
  inverts keep HW.
  unfold WfS.
   rip; eauto.
    unfold STOREM in *. rs.
    rewrite replace_length; eauto.

    (* We can get the old field value that is being replaced. *)
    have (exists svField vField
               ,  get i svs = Some svField
               /\ svalueOf vField svField) as HL
     by  (eapply storet_field_has; eauto).

    destruct HL  as [svField0].
    destruct H12 as [vField0].
    rip. 

    (* The old field value has the same type as the one we're replacing it with *)
    have (TYPE ds nil nil se vField0 (substTTs 0 tsParam tField)) as HF
     by  (eapply storet_field_type; eauto).

    (* When we replace the field the store is still well typed. *)
    eapply storet_replace_field
      with (vField2 := vField); eauto. 
    inverts keep H3. rip.
    eapply TyCon.
     red. burn.
     red. burn.
     eauto.
     simpl. burn.

 Case "EsUpdateSkip".
  exists se. rip.
  unfold xUnit. unfold tUnit.
  inverts keep H2. rip.
  eapply TyCon; simpl.
   red. burn.
   red. burn.
   eauto.
   simpl. burn.
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
    rip. spec IHHS H0.
    rip. shift se'.
    rip. eapply extends_trans; eauto.
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


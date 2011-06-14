


(********************************************************************)
(** * Evaluation Contexts of lists      !!!!!!!!!!! CURRENTLY UNUSED *)
(** An evaluation context defines a function that allows us to
    update a node in the AST for an expression. We use this to
    update nodes during single step evaluation. *)

(*  A context defined by one place in a list of exps, where all 
    the exps to the left of it in the list are values:
         v0 v1 v2 xx x4 x5 x6
                  ^^
    This is useful when enforcing a left-to-right evaluation
    order for a list of exps, like in the arguments of an XCon *)
Inductive exps_ctx : (exp -> list exp) -> Prop :=
 | XscIx 
   :  forall ix xs vs x xs'
   ,  splitAt ix xs = (vs, x :: xs')
   -> Forall  whnfX vs
   -> exps_ctx (fun xx => app vs (xx :: xs')).


Lemma exps_ctx_Forall2 
 :   forall {B: Type} (R: exp -> B -> Prop) 
            (x: exp)  Cs
            (y: B)    (ys: list B)
 ,   exps_ctx Cs
 ->  Forall2 R (Cs x) ys
 ->  (exists y, In y ys /\ R x y).
Proof.
 intros.
 inverts H.
 assert (In x (vs ++ x :: xs')). apply in_splitted.
 lets D: Forall2_exists_left_In H H0.
 destruct D. 
 exists x1. eauto.
Qed.  


Lemma exps_ctx_Forall2_swap
 :   forall {B: Type} (R: exp -> B -> Prop)
            (x1 x2 : exp) Cs
            (y: B)        (ys: list B)
 ,   exps_ctx Cs
 ->  (forall y, R x1 y -> R x2 y)
 ->  Forall2 R (Cs x1) ys
 ->  Forall2 R (Cs x2) ys.
Proof.
 intros.
 inverts H.
 lets HP: (@splitAt_app_cons exp) H2.
 lets D: Forall2_app_inv_l H1.
  destruct D as [ys1].
  destruct H as [ys2].
  inverts H. inverts H5.
  apply Forall2_app.
   auto. clear H1. clear H4.
  destruct ys2.
   inverts H.
   inverts H.
   apply Forall2_cons.
    apply H0. auto.
    auto.
Qed.



Lemma steps_context_XCon_broken1
 :  forall dc xs vs 
 ,  Forall2 STEPS xs vs     -> Forall value vs
 -> STEPS (XCon dc xs) (XCon dc vs).
Proof.
 intros. gen xs.
 induction vs; intros.
  inverts H. auto.
  destruct xs as [xs' | x0].
   inverts H.
   inverts H.
   
   (* This doesn't work. 
      IHvs : Forall value vs ->
             forall xs : list exp,
             Forall2 STEPS xs vs -> STEPS (XCon dc xs) (XCon dc vs)

      |- STEPS (XCon dc (x0 :: xs)) (XCon dc (a :: vs))

      We need to prove something about the XCon with all the final values
      in the final position. The induction is over the number of arguments
      that have already been evaluated, not the number of expressions
      applied to the constructor.
    *)
    admit.
Qed.

Lemma steps_con
 : forall xs vs dc 
 ,  Forall2 STEPS xs vs 
 -> Forall  value vs
 -> STEPS (XCon dc xs) (XCon dc vs).
Proof.
 intros.
 induction vs.
  inverts H. auto.

  destruct xs. 
   inverts H.

   assert (a :: vs = app nil (a :: vs)). 
    simpl. auto. rewrite H1. clear H1.
   eapply (steps_con3 0 e a) with (xs' := xs). simpl. auto.
   auto.
   inverts H. auto.
   inverts H. auto.
Qed.



(* Reduce one of the arguments of a data constructor, 
   then continue reducing the rest. *)
Lemma steps_con3
 :  forall ix vs xs xs' vs' dc x v
 ,  splitAt ix xs = (vs, x :: xs')
 -> Forall  value vs 
 -> STEPS   x v             -> value v
 -> Forall2 STEPS xs' vs'   -> Forall  value vs'
 -> STEPS (XCon dc xs) (XCon dc (vs ++ (v :: vs'))).
Proof.
 intros. gen x xs xs' vs vs' dc.
 induction ix; intros.

 Case "ix = 0".
  lets HS: H. unfold splitAt in HS.
  inversion HS. simpl.
  eapply EsAppend.

   (* do the first eval, STEP x v *)
   lets HC: steps_context XcCon.
    eapply XscIx with (ix := 0); eauto.
   lets HC2: HC H1. rewrite <- H6 in HC2. simpl in HC2.
   eauto.

   (* do the reset, Forall2 STEPS xs' vs' *)
   induction H3.
    auto.
   gen vs'.
   induction xs'; intros.
    inverts H3. auto.
    inverts H3.
   
 
 Case "ix = S ix'".
  lets HX: (@splitAt_app exp) H.
  rewrite HX.

  (* do the first eval *)
  eapply EsAppend.
   lets D: steps_context XcCon. 
    eapply XscIx with (ix := List.length vs)
                      (vs := vs).
    skip. 
    auto.
   lets D1: D H0. eapply D1.

  (* do the rest, need to reassociate ++ *)
  destruct vs' as [vs2 | v2].
   SCase "vs' = nil".
    inverts H2. auto.

   SCase "vs' = vs2 :: v2".
    assert (app vs (v :: v2 :: vs') = app (app vs (v :: nil)) (v2::vs')).
     admit.
    rewrite H6.

   inverts H2.
   eapply IHix.
    eauto.
    inverts H3. eauto.
    admit. (* ok *)
    eauto.
    inverts H3. eauto. clear IHix.
    rewrite H5 in H4.

    assert ( splitAt ix nil (app vs (v :: x0 :: l0))
           = Some (app nil (app vs (v :: nil)), x0 :: l0)).
     simpl.

    (* we've gone the wrong way wirh the rewrite *)
    (* almosted worked though.
        splitAt ix acc (acc ++ xx, yy) => ix == length ix
       this is all the info we should need from the hyp 
      
       do more lemmas involving splitAt
       maybe better to use:
       splitAt ix xx = (take ix xx, drop ix xx)

       .. or not, because we don't want ix in the right
       of the definition. Just do this as a lemma.
     *)

    admit.
    eapply H2.

 
 (********* junk *)
  admit.

  assert (xs = app vs xs'). admit. rewrite H3. clear H3.
 
  destruct xs'.
   admit.
   eapply EsAppend.
    lets D: steps_context XcCon. eapply XscIx. eauto. eauto.
    destruct vs'.
     inverts H0.
     inverts H0.
      eapply D. eauto.


   assert (xs = app vs (e :: xs')). admit. (* ok, prop of splitAt *)
   eapply EsAppend.
   inverts H0.
    eapply steps_con4 
      with (xs' := xs') (ix  := S ix) (x := e). eauto. eauto.
    
     rewrite <- H3. 
      assert (app nil vs = vs). simpl. auto.
      rewrite <- H0 in H2. eauto.
      simpl. 
      rewrite <- H3. clear H3.

      destruct xs'.
       admit.
       assert (splitAt ix nil xs = Some (app vs (e :: nil), e0:: xs')).
        admit. clear H2.

      destruct vs'  as [vs2 | v0]. inverts H0.
      destruct vs'  as [vs3 | v1]. inverts H0. inverts H8.
     
      assert (xs = app vs (e :: e0 :: xs')).
       admit. rewrite H2.

     eapply EsAppend.
     eapply IHix.
      eauto. rewrite <- H2. eauto.

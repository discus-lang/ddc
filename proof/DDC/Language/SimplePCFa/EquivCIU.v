
Require Export DDC.Language.SimplePCFa.Eval.
Require Export DDC.Language.SimplePCFa.StepBase.
Require Export DDC.Language.SimplePCFa.TyJudge.
Require Export DDC.Language.SimplePCFa.ExpLower.
Require Export DDC.Language.SimplePCFa.ExpLift.
Require Export DDC.Language.SimplePCFa.ExpSwap.
Require Export DDC.Language.SimplePCFa.ExpRefs.
Require Export DDC.Language.SimplePCFa.ExpSubsts.
Require DDC.Base.


(* Expressions are ciu-equivalent at the given type
   c.i.u = closed instantiation of use.
   For two open terms, 
     when we substitution any sets of closing values into them,
     then the terminatinon behavior of the two terms is the same
     in all contexts (the use)
*)
Definition EQCIU (te: tyenv) (x1 x2: exp) (t: ty)
 := TYPEX te x1 t
 -> TYPEX te x2 t
 -> (forall f vs x1' x2'
      ,  Forall2 (fun v => TYPEX nil (XVal v)) vs te    
      -> csubstVXs vs x1 x1'
      -> csubstVXs vs x2 x2'
      -> (TERMF f x1' <-> TERMF f x2')).


Lemma eqciu_from_eval
 :  forall te x1 x2 t
 ,  TYPEX te x1 t
 -> TYPEX te x2 t
 -> (    (exists x1', EVAL x1 x1') 
     <-> (exists x2', EVAL x2 x2') )
 -> EQCIU te x1 x2 t.
Proof.
 intros.
 red. rip.
 split; intros;
  eapply eval_term_wrapped; eauto.
Qed.


Lemma eqciu_from_eval'
 :  forall te x1 x2 t
 ,  TYPEX te x1 t
 -> TYPEX te x2 t 
 -> (forall f, (exists x3, EVAL (wrap f x1) x3) 
           <-> (exists x4, EVAL (wrap f x2) x4))
 -> EQCIU te x1 x2 t.
Proof.
 intros.
 red. rip.
 admit.         (* TODO *)
Qed.


Lemma eqciu_if_true
 :  forall te x x1 x2 t
 ,  x = XIf (VConst (CBool true)) x1 x2
 -> TYPEX te x    t
 -> EQCIU te x x1 t.
Proof.
 intros. subst.
 red. rip. inverts H0. burn.
Qed.



(* Nest two let bindings, 
   changes binding structure but not order of operations.

    let [t1] = x1 in let [t2] = x2   in x3
 => let [t2] = (let [t1] = x1 in x2) in x3
*)
Lemma nest_type
 : forall te z1 z2 t x1 t1 x2 t2 x3
 ,  z1 = XLet t1 x1 (XLet t2 x2 x3)
 -> z2 = XLet t2 (XLet t1 (liftXX 1 x1) (swapXX 0 x2)) (lowerXX 1 x3)
 -> ~(refsXX 1 x3)
 -> TYPEX te z1 t
 -> TYPEX te z2 t.
Proof.
 intros. subst.
 eapply TxLet.
 inverts H2. inverts H6.
 eapply type_tyenv_delete with (ix := 1) in H5. 
  simpl in H5. auto. auto.
Qed.


Lemma eciu_if_let_let_nest
 :  forall te z1 z2 t x1 t1 x2 t2 x3
 ,  z1 = XLet t1 x1 (XLet t2 x2 x3)
 -> z2 = XLet t2 (XLet t1 (liftXX 1 x1) (swapXX 0 x2)) (lowerXX 1 x3)
 -> ~ (refsXX 1 x3)
 -> TYPEX te z1 t
 -> EQCIU te z1 z2 t.
Proof.
 intros. subst. red. intros.
 unfold csubstVXs in H4.
 unfold csubstVXs in H5. rip.
 split.

 intros.
  simpl.
  induction f. admit.
  simpl in IHf.
  simpl in H4.
  eapply IHf.

simpl in H4.
  eapply RfLetPush.
  eapply RfLetPush.
  inverts H4.
   admit.       (* fixme *)
   inverts H5.

 intros.
  simpl. 
  simpl in H4.
  inverts H4. 
   admit.      (* fixme *)
   inverts H5.
Qed.

(*

(* TODO: finish me, swap indices 0 1 *)
Definition swapXX (d: nat) (x: exp) : exp
 := x.

(* Swap two let bindings, 
   changes the order of operations. *)
Lemma ecui_if_let_let_swap
 :  forall z1 z2 t1 x1 t2 x2 t3 x3
 ,  z1 = XLet t1 x1             (XLet t2 x2            x3)
 -> z2 = XLet t2 (lowerXX 0 x2) (XLet t1 (liftXX 0 x1) (swapXX 0 x3))
 -> wfX 0 x2
 -> TYPEX nil z1 t3
 -> EQCIU  z1  z2 t3.
Proof.
 intros. subst.
 red. rip.
  admit. (* z2 has same type *)

  split.
   intros.

  
 (* TODO *)
 admit.
 admit.
Qed.

*)


Require Export DDC.Language.SimplePCFa.TyJudge.
Require Export DDC.Language.SimplePCFa.Step.
Require DDC.Base.


(* Expressions are ciu-equivalent at the given type. *)
Definition ECIU (x1 x2 : exp) (t: ty)
 := TYPEX nil x1 t 
 /\ TYPEX nil x2 t
 /\ (forall f, TERMF f x1 <-> TERMF f x2).


Lemma ecui_if_true
 :  forall x x1 x2 t
 ,  x = XIf (VConst (CBool true)) x1 x2
 -> TYPEX nil x    t
 -> ECIU      x x1 t.
Proof.
 intros. subst.
 red. rip. inverts H0. burn.

 split.
  intros.
  inverts H.
  inverts H1. auto.

  intros. eauto.
Qed.



(* TODO: finish me, lower indices (S i) to i *)
Definition lowerXX (d: nat) (x: exp) : exp
 := x.



(* Nest two let bindings, 
   changes binding structure but not order of operations. *)
Lemma eciu_if_let_let_nest
 :  forall z1 z2 t1 x1 t2 x2 t3 x3
 ,  z1 = XLet t1 x1             (XLet t2 x2             x3)
 -> z2 = XLet t2 (XLet t1 x1 (lowerXX 0 x2)) (lowerXX 0 x3)
 -> wfX 0 x2 
              (* TODO: and x3 doesn't ref x2 *)
 -> TYPEX nil z1 t3
 -> ECIU  z1  z2 t3.
Proof.
 admit.  (* TODO *)
Qed.



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
 -> ECIU  z1  z2 t3.
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




Require Import TyJudge.
Require Import EvJudge.
Require Import SubValueValue.
Require Import SubTypeValue.

(** Well formed expresions don't have type binders that shadow
    other variables. 
    TODO: Still have to show that when eval takes a step, can get
          the resulting term back to a well formed one.
**)
Inductive wellformedX : exp -> Prop :=
 | WFX_var
   :  forall v
   ,  wellformedX (XVar v)

 | WFX_lam
   :  forall x T1 t2
   ,  wellformedX (XLam x T1 t2)

 | WFX_app
   : forall t1 t2
   ,  wellformedX t1 
   -> wellformedX t2
   -> wellformedX (XApp t1 t2)

 | WFX_LAM 
   :  forall a t12
   ,  ~bindsX a t12
   -> wellformedX (XLAM a t12)

 | WFX_APP
   :  forall t1 T2
   ,  wellformedX t1
   -> wellformedX (XAPP t1 T2).

Hint Constructors wellformedX.


(* When a well typed term transitions to the next state, 
   its type is perserved.
 *)
Theorem preservation
 :  forall kenv tenv t t' T
 ,  closedX t -> wellformedX t
 -> TYPE kenv tenv t  T
 -> STEP t t'
 -> TYPE kenv tenv t' T.
Proof.
 intros kenv tenv t t' T.
 intros HcX HwX Htype Hstep.
 gen t' T.
 induction t. 

 Case "XVar". 
  intros. inversions Hstep.

 Case "XLam".
  intros. inversions Hstep.

 Case "XApp".
  intros. inversions Hstep.
  SCase "EVAppAbs".
   inversions Htype. inversions H4.
   eapply subst_value_value; eauto.
  SCase "EVApp1".
   inversions Htype.
   eapply TYApp. apply IHt1.
   eauto. inversions HwX. auto. auto. eauto. eauto.
 eauto.
  SCase "EVApp2".
   inversions Htype.
   eapply TYApp. eauto. apply IHt2.
   eauto. inversions HwX. auto. auto. auto.

 Case "XLAM".
  intros. inversions Hstep.

 Case "XAPP".
  intros.
  inversions HwX.
  inverts keep Hstep.
  inverts keep Htype. 
  inverts keep H6.
  SCase "EVAPPLAM".
   lets Hs: subst_type_value H11 H8.
    auto. auto. inversions H0. auto.
   eapply type_tyenv_invariance.
    apply Hs. eauto.
    intro. intro. intro. 
      contradict H1.
      lets C1: closedX_APP_elim1 HcX.
      unfold closedX in C1. specialize C1 with x.

      assert (valname x -> freeX x (substTX a t0 t12) -> freeX x t12).
       admit. (** easy: x cannot be in t0  **)

      contradict C1. 
      lets D1: H1 H C1.
      apply FreeX_LAM. auto. auto.

  SCase "t1 steps".
   inversions Htype.
   eapply TYAPP. auto. 
   apply IHt; eauto. auto. 
Qed.   

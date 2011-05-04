
Require Import SubstTypeType.
Require Export Exp.
Require Export TyEnv.
Require Export KiJudge.


(* Check well typeness of terms. *)
Inductive TYPE : kienv -> tyenv -> exp -> ty -> Prop :=
 | TYVar 
   :  forall i ke te t
   ,  get te i = Some t
   -> KIND ke t  KStar
   -> TYPE ke te (XVar i) t

 | TYLam 
   :  forall ke te x12 t11 t12
   ,  KIND ke t11 KStar
   -> TYPE ke (te :> t11)  x12            t12
   -> TYPE ke  te         (XLam t11 x12) (tFun t11 t12)

 | TYApp 
   :  forall ke te x1 x2 t11 t12
   ,  TYPE ke te x1 (tFun t11 t12) 
   -> TYPE ke te x2 t11
   -> TYPE ke te (XApp x1 x2) t12

 | TYLAM
   :  forall ke te x1 t1
   ,  TYPE (ke :> KStar) (liftTE 0 te) x1        t1
   -> TYPE ke            te           (XLAM x1) (TForall t1)

 | TYAPP
   :  forall ke te x1 t1 t2
   ,  TYPE ke te x1 (TForall t1)
   -> KIND ke t2 KStar
   -> TYPE ke te (XAPP x1 t2) (substTT 0 t2 t1). 

Hint Constructors TYPE.


(* Well Formed ******************************************************)
(* The type produced by a type judgement is well kinded *)
Theorem type_kind
 :  forall ke te x t
 ,  TYPE ke te x t
 -> KIND ke t KStar.
Proof.
 intros. gen ke te t.
 induction x; intros; inverts H; eauto.
 
 Case "XAPP".
  apply IHx in H4. inverts H4.
  eapply subst_type_type; eauto.

 Case "XLam".
  unfold tFun.
  apply IHx in H6.
  eapply KIApp.
   eapply KIApp.
   eauto. auto. auto.

 Case "XApp".
  apply IHx1 in H4.
  apply IHx2 in H6.
   unfold tFun in H4.
    inverts H4.
    inverts H2.
    inverts H3.
    auto.
Qed.


(* A well typed expression is well formed *)
Theorem type_wfX
 :  forall ke te x t
 ,  TYPE ke te x t
 -> wfX  ke te x.
Proof.
 intros. gen ke te t.
 induction x; intros; simpl.

 Case "XVar".
  inverts H. eauto.

 Case "XLAM".
  inverts H.
  apply IHx in H3. eauto.

 Case "XAPP".
  inverts H. 
  lets D: IHx H4. split. 
   auto. eapply kind_wfT. eauto.

 Case "XLam".
  inverts H.
  apply IHx in H6.
  apply kind_wfT in H4. auto.

 Case "XApp".
  inverts H.
  apply IHx1 in H4. 
  apply IHx2 in H6.
   auto.
Qed.
Hint Resolve type_wfX.



Require Export DDC.Language.SystemF2Effect.Ki.
Require Export DDC.Language.SystemF2Effect.TyCon.


(********************************************************************)
(* Type Expressions. *)
Inductive ty  : Type :=
 | TCon      : tycon -> ty
 | TVar      : nat   -> ty
 | TForall   : ki    -> ty -> ty
 | TApp      : ty    -> ty -> ty
 | TSum      : ty    -> ty -> ty
 | TBot      : ki    -> ty.
Hint Constructors ty.


(* Baked-in Value Types *)
Definition tFun   t1 eff t2     := TApp (TApp (TApp (TCon TyConFun) t1) eff) t2.
Definition tUnit                := TCon TyConUnit.
Definition tNat                 := TCon TyConNat.
Definition tBool                := TCon TyConBool.
Definition tRef   r1 t2         := TApp (TApp (TCon TyConRef) r1) t2.


(* Baked-in Effect Types *)
Definition tRead    r1          := TApp (TCon TyConRead)  r1.
Definition tWrite   r1          := TApp (TCon TyConWrite) r1.
Definition tAlloc   r1          := TApp (TCon TyConAlloc) r1.


(********************************************************************)
(* Type Utils *)

(* Get the type constructor of a type, if any *)
Fixpoint getCtorOfType (tt: ty) : option tycon :=
 match tt with
 | TCon tc   => Some tc
 | TApp t1 _ => getCtorOfType t1
 | _         => None
 end.


(* Construct a type application from a constructor type
   and a list of argument types. *)
Fixpoint makeTApps (t1: ty) (tt: list ty) : ty :=
 match tt with
 | nil     => t1
 | t :: ts => makeTApps (TApp t1 t) ts
 end.


Fixpoint takeTCon (tt: ty) : ty :=
 match tt with 
 | TApp t1 t2 => takeTCon t1
 | _          => tt
 end.

Fixpoint takeTArgs (tt: ty) : list ty :=
 match tt with 
 | TApp t1 t2 => snoc t2 (takeTArgs t1)
 | _          => cons tt nil
 end.


(* Break apart a type application into the constructor type
   and a list of argument types. *)
Definition takeTApps (tt: ty) : (ty * list ty) 
 := (takeTCon tt, takeTArgs tt).



Lemma makeTApps_snoc
 : forall t1 t2 t3 ts
 , makeTApps (TApp t1 t2) (snoc t3 ts) 
 = TApp (makeTApps t1 (cons t2 ts)) t3.
Proof.
 intros. gen t1 t2.
 induction ts; simpl; burn.
Qed.


Lemma makeTApps_snoc'
 :  forall t1 t2 ts
 ,  makeTApps t1 (snoc t2 ts)
 =  TApp (makeTApps t1 ts) t2.
Proof.
 intros. gen t1 t2.
 induction ts; intros.
  auto.
  simpl. auto.
Qed.


Lemma takeTCon_makeTApps
 :  forall t1 ts
 ,  takeTCon (makeTApps t1 ts) = takeTCon t1.
Proof.
 intros. gen t1.
 induction ts; intros; simpl; auto.
  rewrite IHts. burn.
Qed.    
Hint Resolve takeTCon_makeTApps.


Lemma makeTApps_takeTCon
 :  forall t1 t2 ts  
 ,  makeTApps t1 ts = t2
 -> takeTCon t1     = takeTCon t2.
Proof.
 intros. gen t1 t2.
 induction ts; intros.
  simpl in H. subst. auto.
  eapply IHts in H. simpl in H. auto.
Qed.
Hint Resolve makeTApps_takeTCon.


Lemma getCtorOfType_makeTApps
 :  forall tc t1 ts
 ,  getCtorOfType t1 = Some tc
 -> getCtorOfType (makeTApps t1 ts) = Some tc.
Proof.
 intros. gen t1.
 induction ts; intros.
  auto. rs.
Qed.
Hint Resolve getCtorOfType_makeTApps.


Lemma makeTApps_rewind
 :  forall t1 t2 ts
 ,  makeTApps (TApp t1 t2) ts = makeTApps t1 (t2 :: ts).
Proof. intros. auto. Qed.


Lemma makeTApps_tycon_eq
 :  forall tc1 tc2 ts1 ts2
 ,  makeTApps (TCon tc1) ts1 = makeTApps (TCon tc2) ts2
 -> tc1 = tc2.
Proof.
 intros.
 assert ( takeTCon (makeTApps (TCon tc1) ts1) 
        = takeTCon (makeTApps (TCon tc2) ts2)) as HT by rs.
 repeat (rewrite takeTCon_makeTApps in HT).
 simpl in HT. inverts HT. auto.
Qed.


Lemma makeTApps_args_eq
 :  forall tc ts1 ts2
 ,  makeTApps (TCon tc) ts1  = makeTApps (TCon tc) ts2
 -> ts1 = ts2.
Proof.
 intros. gen ts2.
 induction ts1 using rev_ind; intros.
  Case "ts1 = nil".
   simpl in H. 
   destruct ts2.
    SCase "ts2 ~ nil".
     auto.

    SCase "ts2 ~ cons".
    simpl in H.
    lets D: @snocable ty ts2. inverts D.
     simpl in H. nope.
     destruct H0 as [t2].
     destruct H0 as [ts2']. 
     subst.
     rewrite makeTApps_snoc in H. nope.
   
  Case "ts1 ~ snoc".
   lets D: @snocable ty ts2. inverts D.
   SCase "ts2 ~ nil".
    simpl in H.
    rewrite app_snoc in H.
    rewrite app_nil_right in H.
    rewrite makeTApps_snoc' in H.
    nope.

   SCase "ts2 ~ snoc" .
    dest t. dest ts'. subst.
    rewrite app_snoc in H.
    rewrite app_snoc. rr.
    rewrite makeTApps_snoc' in H.
    rewrite makeTApps_snoc' in H.
    inverts H.
    eapply IHts1 in H1. subst. 
    auto.
Qed.


Lemma makeTApps_eq_params
 : forall tc1 tc2 ts1 ts2
 ,  makeTApps (TCon tc1) ts1 = makeTApps (TCon tc2) ts2
 -> tc1 = tc2 /\ ts1 = ts2.
Proof.
 intros.
 assert (tc1 = tc2).
  eapply makeTApps_tycon_eq; eauto.
  subst.
 assert (ts1 = ts2).
  eapply makeTApps_args_eq; eauto.
  subst.
 auto.
Qed.


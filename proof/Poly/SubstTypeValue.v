
Require Import SubstTypeType.
Require Import TyJudge.
Require Import KiJudge.
Require Import Exp.
Require Import Env.
Require Import Base.


(* Lift type indices that are at least a certain depth. *)
Fixpoint liftTX' (d: nat) (xx: exp) : exp :=
  match xx with
  |  XVar _     => xx

  |  XLAM x     
  => XLAM (liftTX' (S d) x)

  |  XAPP x t 
  => XAPP (liftTX' d x)  (liftTT' d t)
 
  |  XLam t x   
  => XLam (liftTT' d t)  (liftTX' d x)

  |  XApp x1 x2
  => XApp (liftTX' d x1) (liftTX' d x2)
 end.


(* Substitution of Types in Exps *)
Fixpoint substTX' (d: nat) (u: ty) (xx: exp) : exp :=
  match xx with
  | XVar _     => xx

  |  XLAM x     
  => XLAM (substTX' (S d) (liftTT u) x)

  |  XAPP x t
  => XAPP (substTX' d u x)  (substTT' d u t)

  |  XLam t x
  => XLam (substTT' d u t)  (substTX' d u x)

  |  XApp x1 x2
  => XApp (substTX' d u x1) (substTX' d u x2)
 end.


Definition  substTX := substTX' 0.
Hint Unfold substTX.

Inductive substTEF : ty -> ty -> env ty -> env ty -> Prop :=
 | SUBSTFUCKER 
   : forall t1 t2 e1 e2, substTEF t1 t2 e1 e2.



Theorem subst_type_value_drop
 :  forall ix ke te x1 t1 t2 k2
 ,  get ke ix = Some k2
 -> TYPE ke  te x1 t1
 -> KIND (drop ix ke)  t2 k2
 -> TYPE (drop ix ke) 
         (substTE' ix t2 te)
         (substTX' ix t2 x1)
         (substTT' ix t2 t1).
Proof.
 intros. gen ix ke te t1 t2 k2.
 induction x1; intros; simpl; inverts H0; eauto.

 Case "XVar".
  apply TYVar. admit. (* ok, lemma about map *)

 Case "XLAM".
  simpl.
  apply TYLAM. rewrite drop_rewind.
  
  assert (liftTE (substTE' ix t2 te) = substTE' (S ix) (liftTT' 0 t2) (liftTE' 0 te)).
    admit. rewrite H0. (* ok, instance of liftTT_substTT *)

  eapply IHx1.
   unfold liftTE in H5. auto. 
   simpl. eauto.
   simpl. apply liftTT_push. auto.

 Case "XAPP".
 

 
  

   





























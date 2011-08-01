
Require Export DDC.Language.SystemF2.TyBase.
Require Export DDC.Language.SystemF2.TyLift.
Require Export DDC.Language.SystemF2.TySubst.


(* Well formed types are closed under the given kind environment. *)
Inductive wfT (ke: kienv) : ty -> Prop :=
 | WfT_TVar 
   :  forall i
   ,  (exists k, get i ke = Some k)
   -> wfT ke (TVar i)

 | WfT_TCon
   :  forall n
   ,  wfT ke (TCon n)

 | WfT_TForall
   :  forall t
   ,  wfT (ke :> KStar) t
   -> wfT ke (TForall t)

 | WfT_TApp
   :  forall t1 t2
   ,  wfT ke t1 -> wfT ke t2
   -> wfT ke (TApp t1 t2).
Hint Constructors wfT.


(* Closed types are well formed under an empty environment. *)
Definition closedT (tt: ty) : Prop
 := wfT nil tt.
Hint Unfold closedT.








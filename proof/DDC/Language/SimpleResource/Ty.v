
Require Export DDC.Base.

(********************************************************************)
(* Resources *)
Inductive res : Type :=
 | RZero  : res
 | RVar   : nat -> res
 | RNat   : nat -> res
 | RAdd   : res -> res -> res
 | RMul   : res -> res -> res.

Inductive wfR (rn: nat) : res -> Type :=
 | WfR_RVar 
   :  forall ri
   ,  wfR rn (RVar ri)

 | WfR_RNat
   :  forall n
   ,  wfR rn (RNat n)

 | WfR_RAdd
   :  forall r1 r2
   ,  wfR rn r1 -> wfR rn r2
   -> wfR rn (RAdd r1 r2)

 | WfR_RMul
   :  forall r1 r2
   ,  wfR rn r1 -> wfR rn r2
   -> wfR rn (RMul r1 r2).



(********************************************************************)
(* Types *)
Inductive ty  : Type :=
 (* Type variable *)
 | TVar     : nat -> ty

 (* Type constructor for scalar types *)
 | TCon     : nat -> ty

 (* Array types. *)
 | TArray   : res -> ty -> ty

 (* Function type with a work and space usage *)
 | TFun     : ty  -> ty -> res -> res -> ty

 (* Type Abstracton *) 
 | TForallT  : ty  -> ty

 (* Resource Abstraction *)
 | TForallR : ty  -> ty.
Hint Constructors ty.


(* Type Environments *)
Definition tyenv := list ty.


(* Well formed types *)
Inductive wfT (kn: nat) (rn: nat) : ty -> Prop :=
 | WfT_TVar 
   :  forall i
   ,  i < kn
   -> wfT kn rn (TVar i)

 | WfT_TCon
   :  forall c
   ,  wfT kn rn (TCon c)

 | WfT_TArray
   :  forall r1 t1
   ,  wfR rn r1
   -> wfT kn rn (TArray r1 t1)

 | WfT_TFun 
   :  forall t1 t2 w1 s1
   ,  wfT kn rn t1 -> wfT kn rn t2
   -> wfR rn w1    -> wfR rn s1
   -> wfT kn rn (TFun t1 t2 w1 s1)

 | WfT_TForall 
   :  forall t1
   ,  wfT (S kn) rn t1
   -> wfT kn rn (TForallT t1)

 | WfT_TForallr
   :  forall t1
   ,  wfT kn (S rn) t1
   -> wfT kn rn (TForallR t1).


(********************************************************************)
(* Buildins *)

Definition tUnit := TCon 0.
Definition tBool := TCon 1.
Definition tInt  := TCon 2.

(* replicate 
    :: forall a. forallr n. a -> Array n a | n | n *)
Definition tReplicate 
 := TForallT (TForallR 
     (TFun (TVar 0) (TArray (RVar 0) (TVar 0))
           (RVar 0) (RVar 0))).

(* sum       
    :: forallr n. Array n Int -> Int | n | 1 *)
Definition tSum
 := TForallR 
     (TFun (TArray (RVar 0) tInt) tInt
           (RVar 0) (RVar 0)).

(* map       
    :: forall a b. forallr w s n
    .  (a -> b | w | s) -> Array n a -> Array n b | n * w | n * s *)
Definition tMap 
 := TForallT (TForallT (TForallR 
     (TFun (TFun (TVar 1) (TVar 0) (RVar 2) (RVar 1))
           (TFun (TArray (RVar 0) (TVar 1))
                 (TArray (RVar 0) (TVar 0))
                 (RMul (RVar 0) (RVar 2))
                 (RMul (RVar 0) (RVar 1)))
           RZero
           RZero))).                        

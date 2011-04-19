
Require Export Ty.
Require Export Env.
Require Import Base.

(* Kinds of types ***************************************************)
Inductive KIND : kienv -> ty -> ki -> Prop :=
 | KICon 
   :  forall kenv c
   ,  KIND kenv (TCon c) KStar

 | KIVar
   :  forall kenv it k
   ,  get kenv it = Some k
   -> KIND kenv (TVar it) k

 | KIForall
   :  forall kenv t
   ,  KIND (kenv :> KStar) t           KStar
   -> KIND kenv            (TForall t) KStar

 | KIFun 
   :  forall kenv t1 t2
   ,  KIND kenv t1 KStar -> KIND kenv t2 KStar
   -> KIND kenv (TFun t1 t2) KStar.
Hint Constructors KIND.




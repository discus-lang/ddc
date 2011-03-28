
Require Import KiJudge.
Require Import TyJudge.

(* Substitution of types in values preserves typing.
 *)
Lemma subst_type_value
 :  forall kenv tenv a t1 t2 T1 T2 K2
 ,  (forall z, freeX z t2 -> freshX z t1)
 -> TYPE (extend kenv a K2) tenv  t1  T1
 -> KIND      kenv                T2  K2
 -> TYPE kenv tenv  (substTX a T2 t1) T1.  (* TODO, sub into tyenv *)
Proof.
 intros kenv tenv a t1 t2 T1 T2 K2.
 generalize dependent kenv.
 generalize dependent tenv.
 generalize dependent T1.
 induction t1.
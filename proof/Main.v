
Require Import Base.
Require Import Name.
Require Import Exp.
Require Import Ty.

(* evaluation *********************************************)
Inductive STEP : exp -> exp -> Prop := 
  | EVAppAbs 
    :   forall x T t12 v2 
    ,   VALUE v2 
    ->  STEP  (XApp (XLam x T t12) v2)
              (subst x v2 t12)

  | EVApp1 
    :   forall t1 t1' t2
    ,   STEP t1 t1'
    ->  STEP (XApp t1 t2) (XApp t1' t2)

  | EVApp2
    :   forall v1 t2 t2'
    ,   VALUE v1
    ->  STEP  t2 t2'
    ->  STEP  (XApp v1 t2) (XApp v1 t2').


Example xIdA := XLam nA tA (XVar nA).

Lemma example_step1 : STEP (XApp xIdA (XAtom nB)) (XAtom nB).
Proof. unfold xIdA. apply EVAppAbs. auto. Qed.






Require Import Base.
Require Import Name.
Require Import Exp.
Require Import Ty.
Require Import Context.

(* evaluation *********************************************)


Hint Constructors STEP.


(* example expressions ************************************)
Example xId_A := XLam nA tA (XVar nA).
Example xK_AB := XLam nA tA (XLam nB tB (XVar nA)). 

Lemma example_step1 : STEP (XApp xId_A (XAtom nB)) (XAtom nB).
Proof. unfold xId_A. apply EVAppAbs. auto. Qed.




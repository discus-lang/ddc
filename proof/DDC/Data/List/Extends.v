
Require Import DDC.Data.List.Base.


Definition extends {A: Type} (xs: list A) (ys: list A)
 := exists zs, xs = zs >< ys.


Lemma extends_refl
 : forall {A: Type} (se: list A)
 , extends se se.
Proof.
 induction se; intros.
  unfold extends. exists (@nil A). eauto.
  unfold extends. exists (@nil A). eauto.
Qed.
Hint Resolve extends_refl.


Lemma extends_snoc 
 : forall {A: Type} x (xx: list A)
 , extends (x <: xx) xx.
Proof.
 intros. unfold extends.
 exists (x :: nil).
 induction xx.
  simpl. auto.
  simpl. rewrite IHxx. auto.
Qed.
Hint Resolve extends_snoc. 



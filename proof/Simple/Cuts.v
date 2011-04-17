
Theorem type_tyenv_invariance
 : forall tenv tenv' t T
 ,  TYPE tenv  t T
 -> (forall n m, coversX m t -> n <= m -> get tenv' n = get tenv n)
 -> TYPE tenv' t T.
Proof.
 intros. gen tenv'. induction H; intros.
 
 eapply TYVar. rewrite <- H. eapply H0 with (S i). eauto. eauto.

 eapply TYLam.
  eapply IHTYPE. intros.
   destruct n. simpl. auto.
   simpl. eapply H0 with (S m).
   eapply CoversX_lam.
   eapply coversX_weaken_succ.
   eapply coversX_weaken_succ. auto.
   omega.
  
 eapply TYApp.
  eapply IHTYPE1. intros. eapply H1. skip. auto. (* ok, big enough *)
  eapply IHTYPE2. intros. eapply H1. skip. auto. (* ok, big enough *)
Qed.

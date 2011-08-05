
Require Export DDC.Language.SystemF2Data.ExpBase.
Require Export DDC.Language.SystemF2Data.ExpAlt.
Require Export DDC.Base.


(********************************************************************)
(* Lift type indices in expressions. *)
Fixpoint liftTX (d: nat) (xx: exp) : exp :=
  match xx with
  |  XVar _     => xx

  |  XLAM x     
  => XLAM (liftTX (S d) x)

  |  XAPP x t 
  => XAPP (liftTX d x)  (liftTT 1 d t)
 
  |  XLam t x   
  => XLam (liftTT 1 d t)  (liftTX d x)

  |  XApp x1 x2
  => XApp (liftTX d x1) (liftTX d x2)

  |  XCon dc ts xs
  => XCon dc (map (liftTT 1 d) ts) (map (liftTX d) xs)

  |  XCase xx alts
  => XCase (liftTX d xx) (map (liftTA d) alts)
 end

 with liftTA (d: nat) (aa: alt) : alt :=
  match aa with
  |  AAlt dc xx
  => AAlt dc (liftTX d xx)
  end.


(********************************************************************)
(* Lift expression indices in expressions *)
Fixpoint liftXX (n:  nat) (d:  nat) (xx: exp) {struct xx} : exp :=
 match xx with 
    |  XVar ix    
    => if le_gt_dec d ix
        (* index was pointing into env, lift it across new elems *)
        then XVar (ix + n)
        (* index was locally bound, leave it be *)
        else xx

    |  XLAM x
    => XLAM (liftXX n d x)

    |  XAPP x t
    => XAPP (liftXX n d x) t

    (* increase the depth as we move across a lambda *)
    |  XLam t1 x1
    => XLam t1 (liftXX n (S d) x1)

    |  XApp x1 x2
    => XApp   (liftXX n d x1) (liftXX n d x2)

    (* lift all the arguments of a data constructor *)
    |  XCon dc ts xs
    => XCon dc ts (map (liftXX n d) xs)

    (* lift all the alternatives in a case-expression *)
    |  XCase x alts
    => XCase (liftXX n d x) (map (liftXA n d) alts)
    end

 with liftXA (n: nat) (d: nat) (aa: alt) {struct aa}:= 
  match aa with
  (* When we enter into the right of an alternative, a new type
     is pushed onto the environment for each of the arguments
     of the data constructor. We need to increase the current
     binding depth by the number of arguments. *)
  |  AAlt (DataCon tag arity) x 
  => AAlt (DataCon tag arity) (liftXX n (d + arity) x)
  end.


(* The data constructor of an alternative is unchanged
   by lifting. *)
Lemma dcOfAlt_liftXA
 : forall n d a
 , dcOfAlt (liftXA n d a) = dcOfAlt a.
Proof.
 intros. destruct a. destruct d0. auto.
Qed.


(* When we lift an expression by zero places,
   then the expression is unchanged. *)
Lemma liftXX_zero
 : forall d x
 , liftXX 0 d x = x.
Proof.
 intros. gen d.
 induction x using exp_mutind with 
  (PA := fun a => forall d
      ,  liftXA 0 d a = a)
  ; intros; simpl; try burn.

 Case "XVar".
  lift_cases; intros; auto.

 Case "XCon".
  nforall.
  rewrite (map_ext_in (liftXX 0 d) id); auto.
  rewrite map_id; auto.

 Case "XCase".
  nforall.
  rewrite (map_ext_in (liftXA 0 d) id); auto.
  rewrite map_id. rewrite IHx; auto.

 Case "XAlt".
  destruct dc. burn.
Qed.


(* Commutivity of lifting. *)
Lemma liftXX_comm
 : forall n m x d
 , liftXX n d (liftXX m d x)
 = liftXX m d (liftXX n d x). 
Proof.
 intros. gen d.
 induction x using exp_mutind with 
  (PA := fun a => forall d
      ,  liftXA n d (liftXA m d a)
      =  liftXA m d (liftXA n d a))
  ; intros; simpl; try burn.

 Case "XVar".
  repeat (simple; lift_cases; intros); burn.

 Case "XCon".
  f_equal.
  rewrite map_map.
  rewrite map_map.
  rewrite Forall_forall in H.
  rewrite (map_ext_in 
   (fun x0 => liftXX n d (liftXX m d x0))
   (fun x0 => liftXX m d (liftXX n d x0))).
  auto. eauto.

 Case "XCase".
  f_equal.
  eauto.
  rewrite map_map.
  rewrite map_map.
  rewrite Forall_forall in H.
  rewrite (map_ext_in
   (fun a1 => liftXA n d (liftXA m d a1))
   (fun a1 => liftXA m d (liftXA n d a1))).
  auto. eauto.

 Case "XAlt".
  destruct dc.
  simpl. burn.
Qed.


(* When consecutively lifting an expression, we can lift by one
   more place in the first lifting and but one less in the second. *)
Lemma liftXX_succ
 : forall n m d x
 , liftXX (S n) d (liftXX m     d x)
 = liftXX n     d (liftXX (S m) d x). 
Proof.
 intros. gen d.
 induction x using exp_mutind with 
  (PA := fun a => forall d
      ,  liftXA (S n) d (liftXA  m    d a)
      =  liftXA n     d (liftXA (S m) d a))
  ; intros; simpl; try burn.

 Case "XVar".
  repeat (simple; lift_cases; intros); burn.

 Case "XCon".
  f_equal.
  repeat (rewrite map_map).
  rewrite Forall_forall in H.
  rewrite (map_ext_in
   (fun x0 => liftXX (S n) d (liftXX m d x0))
   (fun x0 => liftXX n d (liftXX (S m) d x0))).
  auto. auto.

 Case "XCase".
  f_equal.
  eauto.
  repeat (rewrite map_map).
  rewrite Forall_forall in H.
  rewrite (map_ext_in
   (fun x1 => liftXA (S n) d (liftXA m d x1))
   (fun x1 => liftXA n d (liftXA (S m) d x1))).
  auto. auto.

 Case "XAlt".
  destruct dc.
  simpl. burn.
Qed.


(* We can collapse two consecutive lifting expressions by lifting 
   just onces by the sum of the places, provided the lifting
   occurs at depth zero. 
   (TODO: we may be able to weaken this second requirement. *)
Lemma liftXX_plus 
 : forall n m x 
 , liftXX n 0 (liftXX m 0 x) = liftXX (n + m) 0 x.
Proof.
 intros. gen n.
 induction m.
  intros. rewrite liftXX_zero. nnat. auto.
  intros.
   assert (n + S m = S n + m). 
    omega. rewrite H. clear H.
   rewrite liftXX_comm.
   rewrite <- IHm.
   rewrite liftXX_comm.
   rewrite liftXX_succ. 
   auto.
Qed.


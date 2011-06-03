
Require Export ExpBase.


(* Lifting **********************************************************)
(* When we push new elements on the environment stack of an
   expression, we need to lift free indices in the expression 
   across the new elements.

   For example given: 
             t1, t0 |- 0 1 (\. 0 1 2) :: t3

   Pushing two more elements gives:
     t1, t0, ta, tb |- 2 3 (\. 0 3 4) :: t3
 *)
Fixpoint 
 liftX  (n:  nat) (* number of elements pushed on stack *)
        (d:  nat) (* current binding depth in expression *)
        (xx: exp) (* expression to lift *)
        {struct xx}
        : exp
 := match xx with 
    |  XVar ix    
    => if le_gt_dec d ix
        (* var was pointing into env, lift it across new elems *)
        then XVar (ix + n)
        (* var was locally bound, leave it be *)
        else xx

    (* increase the depth as we move across a lambda *)
    |  XLam t1 x1
    => XLam t1 (liftX n (S d) x1)

    |  XApp x1 x2
    => XApp   (liftX n d x1) (liftX n d x2)

    |  XCon dc xs
    => XCon dc (List.map (liftX n d) xs)

    |  XCase x alts
    => XCase (liftX n d x) (List.map (liftA n d) alts)

    end

 with liftA (n: nat) (d: nat) (aa: alt) {struct aa}:= 
  match aa with
  | AAlt dc ts x => AAlt dc ts 
                        (liftX  n 
                               (d + List.length ts)
                                x)
  end.


Lemma liftX_zero
 : forall d x
 , liftX 0 d x = x.
Proof.
 intros. gen d.
 induction x using exp_mutind with 
  (PA := fun a => forall d
      ,  liftA 0 d a = a)
  ; intros; simpl.

 Case "XVar".
  lift_cases; intros; auto.

 Case "XLam".
  rewrite IHx. auto.

 Case "XApp".
  rewrite IHx1. rewrite IHx2. auto.

 Case "XCon".
  rewrite Forall_forall in H.
  rewrite (map_ext_In (liftX 0 d) id).
  rewrite map_id. auto. auto.

 Case "XCase".
  rewrite Forall_forall in H.
  rewrite (map_ext_In (liftA 0 d) id).
  rewrite map_id. rewrite IHx. auto. auto.

 Case "AAlt".
  rewrite IHx. auto.
Qed.


Lemma liftX_comm
 : forall n m x d
 , liftX n d (liftX m d x)
 = liftX m d (liftX n d x). 
Proof.
 intros. gen d.
 induction x using exp_mutind with 
  (PA := fun a => forall d
      ,  liftA n d (liftA m d a)
      =  liftA m d (liftA n d a))
  ; intros; simpl.

 Case "XVar".
  repeat (simple; lift_cases; intros; burn).

 Case "XLam".
  rewrite IHx. auto.

 Case "XApp".
  rewrite IHx1. rewrite IHx2. auto.

 Case "XCon".
  f_equal.
  rewrite map_map.
  rewrite map_map.
  rewrite Forall_forall in H.
  rewrite (map_ext_In 
   (fun x0 => liftX n d (liftX m d x0))
   (fun x0 => liftX m d (liftX n d x0))).
  auto. eauto.

 Case "XCase".
  f_equal.
  eauto.
  rewrite map_map.
  rewrite map_map.
  rewrite Forall_forall in H.
  rewrite (map_ext_In
   (fun a1 => liftA n d (liftA m d a1))
   (fun a1 => liftA m d (liftA n d a1))).
  auto. eauto.

 Case "XAlt".
  f_equal.
  eauto.
Qed.


Lemma liftX_succ
 : forall n m d x
 , liftX (S n) d (liftX m     d x)
 = liftX n     d (liftX (S m) d x). 
Proof.
 intros. gen d.
 induction x using exp_mutind with 
  (PA := fun a => forall d
      ,  liftA (S n) d (liftA  m    d a)
      =  liftA n     d (liftA (S m) d a))
  ; intros; simpl.

 Case "XVar".
  repeat (simple; lift_cases; intros; burn).

 Case "XLam".
  f_equal.
  rewrite IHx. auto.

 Case "XApp".
  f_equal.
  rewrite IHx1. auto.
  rewrite IHx2. auto.

 Case "XCon".
  f_equal.
  repeat (rewrite map_map).
  rewrite Forall_forall in H.
  rewrite (map_ext_In
   (fun x0 => liftX (S n) d (liftX m d x0))
   (fun x0 => liftX n d (liftX (S m) d x0))).
  auto. auto.

 Case "XCase".
  f_equal.
  eauto.
  repeat (rewrite map_map).
  rewrite Forall_forall in H.
  rewrite (map_ext_In
   (fun x1 => liftA (S n) d (liftA m d x1))
   (fun x1 => liftA n d (liftA (S m) d x1))).
  auto. auto.

 Case "AAlt".
  f_equal.
  rewrite IHx. auto.
Qed.


Lemma liftX_plus 
 : forall n m x 
 , liftX n 0 (liftX m 0 x) = liftX (n + m) 0 x.
Proof.
 intros. gen n.
 induction m.
  intros. rewrite liftX_zero. nnat. auto.
  intros.
   assert (n + S m = S n + m). 
    omega. rewrite H. clear H.
   rewrite liftX_comm.
   rewrite <- IHm.
   rewrite liftX_comm.
   rewrite liftX_succ. 
   auto.
Qed.
   

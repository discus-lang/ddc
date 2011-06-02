
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
 apply (exp_mutind
         (fun x => forall d, liftX 0 d x = x)
         (fun a => forall d, liftA 0 d a = a))
  ; intros; simpl.

 Case "XVar".
  lift_cases; intros; auto.

 Case "XLam".
  rewrite H. auto.

 Case "XApp".
  rewrite H. rewrite H0. auto.

 Case "XCon".
  rewrite Forall_forall in H.
  rewrite (map_ext_In (liftX 0 d) id).
  rewrite map_id. auto. auto.

 Case "XCase".
  rewrite Forall_forall in H0.
  rewrite (map_ext_In (liftA 0 d) id).
  rewrite map_id. rewrite H. auto. auto.

 Case "AAlt".
  rewrite H. auto.
Qed.



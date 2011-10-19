
Require Export DDC.Language.SystemF2Store.Exp.


(********************************************************************)
(* Storeable values are the ones that we can keep directly in 
   store bindings *)
Inductive svalue :=
 | SLoc  : nat -> svalue
 | SLAM  : exp -> svalue
 | SLam  : ty  -> exp -> svalue.
Hint Constructors svalue.


Definition takeSValueOfExp (xx : exp) : option svalue :=
 match xx with
 | XLoc n    => Some (SLoc n)
 | XLAM x    => Some (SLAM x)
 | XLam t x  => Some (SLam t x)
 | _         => None
 end.


Definition expOfSValue (s: svalue) : exp :=
 match s with
 | SLoc n    => XLoc n
 | SLAM x    => XLAM x
 | SLam t x  => XLam t x
 end.


Definition svalueOf (xx : exp) (sv : svalue) : Prop
 := takeSValueOfExp xx = Some sv.


(* There is an expression for every store value *)
Lemma exp_from_svalue
 : forall sv, exists v, svalueOf v sv.
Proof.
 intros.
 destruct sv.
  exists (XLoc n).   burn.
  exists (XLAM e).   burn.
  exists (XLam t e). burn.
Qed.
Hint Resolve exp_from_svalue.


(* There is a store value for every expression value. *)
Lemma svalue_from_value
 : forall v, value v -> (exists sv, svalueOf v sv).
Proof.
 intros.
 destruct v; nope.
  exists (SLoc  n).  burn.
  exists (SLAM  v).  burn.
  exists (SLam t v). burn.
Qed.
Hint Resolve svalue_from_value.


Lemma svalue_of_expOfSValue
 : forall sv : svalue, svalueOf (expOfSValue sv) sv.
Proof.
 intros. destruct sv; burn.
Qed.
Hint Resolve svalue_of_expOfSValue.


Lemma svalueOf_is_expOfSValue
 :  forall v sv
 ,  svalueOf v sv
 -> v = expOfSValue sv.
Proof.
 intros.
 inverts H.
  destruct sv;
   destruct v; try burn; simpl in *; inverts H1; auto.
Qed.
Hint Resolve svalueOf_is_expOfSValue.


Lemma svalueOf_forall_expOfSValue
 :  forall vs svs
 ,  Forall2 svalueOf vs svs
 -> vs = map expOfSValue svs.
Proof.
 intros. induction H; burn.
Qed.
Hint Resolve svalueOf_forall_expOfSValue.


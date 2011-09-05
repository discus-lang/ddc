
Require Export DDC.Language.SystemF2.Ty.
Require Export DDC.Language.SystemF2Store.Def.
Require Export DDC.Base.

(* Expressions *)
Inductive exp : Type :=
 (* Variables and locations *)
 | XVar    : nat -> exp          (* value variable *)
 | XLoc    : nat -> exp          (* store location *)

 (* Type functions *)
 | XLAM    : exp -> exp          (* type abstraction *)
 | XAPP    : exp -> ty  -> exp   (* type application *)

 (* Value functions *)
 | XLam    : ty  -> exp -> exp   (* function abstraction *)
 | XApp    : exp -> exp -> exp   (* function application *)

 (* Algebraic data *)
 | XCon    : datacon -> list ty -> list exp -> exp
 | XCase   : exp     -> list alt -> exp

 (* Destructive update of store, 
    takes the constructor and argument indices,
    the location to update, and a new expression. *)
 | XUpdate : nat -> nat -> exp -> exp -> exp

 (* Alternatives *)
with alt     : Type :=
 | AAlt   : datacon -> exp -> alt.

Hint Constructors exp.
Hint Constructors alt.


(********************************************************************)
(* Mutual induction principle for expressions.
   As expressions are indirectly mutually recursive with lists,
   Coq's Combined scheme command won't make us a strong enough
   induction principle, so we need to write it out by hand. *)
Theorem exp_mutind
 : forall 
    (PX : exp -> Prop)
    (PA : alt -> Prop)
 ,  (forall n,                                PX (XVar n))
 -> (forall l,                                PX (XLoc l))
 -> (forall x1,      PX x1                 -> PX (XLAM x1))
 -> (forall x1 t2,   PX x1                 -> PX (XAPP x1 t2))
 -> (forall t  x1,   PX x1                 -> PX (XLam t x1))
 -> (forall x1 x2,   PX x1 -> PX x2        -> PX (XApp x1 x2))
 -> (forall dc ts xs,         Forall PX xs -> PX (XCon dc ts xs))
 -> (forall x  aa,   PX x  -> Forall PA aa -> PX (XCase x aa))
 -> (forall c  i x1 x2, PX x1 -> PX x2     -> PX (XUpdate c i x1 x2))
 -> (forall dc x,    PX x                  -> PA (AAlt dc x))
 ->  forall x, PX x.
Proof. 
 intros PX PA.
 intros var loc tlam tapp lam app con case update alt.
 refine (fix  IHX x : PX x := _
         with IHA a : PA a := _
         for  IHX).

 (* expressions *)
 case x; intros.

 Case "XVar".
  apply var.

 Case "XLoc".
  apply loc.

 Case "XLAM".
  apply tlam. 
   apply IHX.

 Case "XAPP".
  apply tapp.
   apply IHX.
 
 Case "XLam".
  apply lam. 
   apply IHX.

 Case "XApp".
  apply app. 
   apply IHX.
   apply IHX.

 Case "XCon".
  apply con.
   induction l0; intuition.

 Case "XCase".
  apply case.
   apply IHX.
   induction l; intuition.

 Case "XUpdate".
  apply update.
   apply IHX.
   apply IHX.
 
 (* alternatives *)
 case a; intros.

 Case "XAlt".
  apply alt.
   apply IHX.
Qed.

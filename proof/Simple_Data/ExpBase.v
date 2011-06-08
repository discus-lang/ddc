
Require Export Base.
Require Export Env.


(* Types ************************************************************)
Inductive tycon : Type :=
 | TyConData   : nat -> tycon.
Hint Constructors tycon.

Inductive ty : Type :=
 | TCon   : tycon -> ty
 | TFun   : ty    -> ty -> ty.
Hint Constructors ty.


(* Expressions ******************************************************
   We use deBruijn indices for binders.
 *)
Inductive datacon : Type :=
 | DataCon    : nat -> datacon.
Hint Constructors datacon.


Inductive exp : Type :=
 (* Functions *)
 | XVar   : nat -> exp
 | XLam   : ty  -> exp -> exp
 | XApp   : exp -> exp -> exp

 (* Data Types *)
 | XCon   : datacon -> list exp -> exp
 | XCase  : exp     -> list alt -> exp

 (* Alternatives *)
with alt     : Type :=
 | AAlt   : datacon -> list ty  -> exp -> alt.

Hint Constructors exp.
Hint Constructors alt.


Fixpoint dcOfAlt (aa: alt) : datacon :=
 match aa with 
 | AAlt dc _ _ => dc
 end.
Hint Unfold dcOfAlt.

(* Mutual induction principle for expressions.
   As expressions are indirectly mutually recursive with lists,
   Coq's Combined scheme command won't make us a strong enough
   induction principle, so we need to write it out by hand. *)
Theorem exp_mutind
 : forall 
    (PX : exp -> Prop)
    (PA : alt -> Prop)
 ,  (forall n,                                PX (XVar n))
 -> (forall t  x1,   PX x1                 -> PX (XLam t x1))
 -> (forall x1 x2,   PX x1 -> PX x2        -> PX (XApp x1 x2))
 -> (forall dc xs,            Forall PX xs -> PX (XCon dc xs))
 -> (forall x  aa,   PX x  -> Forall PA aa -> PX (XCase x aa))
 -> (forall dc ts x, PX x                  -> PA (AAlt dc ts x))
 ->  forall x, PX x.
Proof. 
 intros PX PA.
 intros var lam app con case alt.
 refine (fix  IHX x : PX x := _
         with IHA a : PA a := _
         for  IHX).

 (* expressions *)
 case x; intros.

 Case "XVar".
  apply var.

 Case "XLam".
  apply lam. 
   apply IHX.

 Case "XApp".
  apply app. 
   apply IHX.
   apply IHX.

 Case "XCon".
  apply con.
   induction l; intuition.

 Case "XCase".
  apply case.
   apply IHX.
   induction l; intuition.

 (* alternatives *)
 case a; intros.

 Case "XAlt".
  apply alt.
   apply IHX.
Qed.


(* Definitions ******************************************************)
Inductive def  : Type :=
 (* Definition of a data type constructor *)
 | DefDataType 
   :  tycon        (* Name of data type constructor *)
   -> list datacon (* Data constructors that belong to this type *)
   -> def

 (* Definition of a data constructor *)
 | DefData 
   :  datacon      (* Name of data constructor *)
   -> list ty      (* Types of arguments *)
   -> ty           (* Type  of constructed data *)
   -> def.
Hint Constructors def.


(* Type Environments ************************************************)
Definition tyenv := env ty.
Definition defs  := env def.


Fixpoint getTypeDef (tc: tycon) (ds: defs) : option def := 
 match ds with 
 | ds' :> DefDataType tc' _ as d
 => if tycon_beq tc tc' 
     then  Some d
     else  getTypeDef tc ds'

 | ds' :> _ => getTypeDef tc ds'
 | Empty    => None
 end.


Fixpoint getDataDef (dc: datacon) (ds: defs) : option def := 
 match ds with 
 | ds' :> DefData dc' _ _ as d
 => if datacon_beq dc dc' 
     then  Some d
     else  getDataDef dc ds'

 | ds' :> _ => getDataDef dc ds'
 | Empty    => None
 end.




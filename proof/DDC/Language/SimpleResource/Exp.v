
Require Export DDC.Base.
Require Export DDC.Language.SimpleResource.Ty.
Require Export Coq.Strings.String.


(********************************************************************)
(* Value Expressions *)
Inductive exp : Type :=
 | XInt  : nat    -> exp       (* integer literal *)
 | XBind : string -> exp       (* bound at top level *)
 | XVar  : nat    -> exp       (* deBruijn index for local variable *)

 | XLamT : exp -> exp
 | XAppT : exp -> ty  -> exp

 | XLamR : exp -> exp
 | XAppR : exp -> res -> exp

 | XLam  : ty  -> exp -> exp   (* function abstraction *)
 | XApp  : exp -> exp -> exp   (* function application *)

 | XClo  : forall env, env -> exp -> exp -> exp.


Hint Constructors exp.

(*
Fixpoint vectX (xx : exp) : exp := 
 match xx  with
 | XInt  k     => XInt k
 | XBind n     => XBind n        (* convert to vect version *)
 | XVar  i     => XVar i
 
 | XLamT x     => XLamT (vect x)
 ... 
 | XLam t1 x2  => XClo (XLam (flattenT t1) (vectX x2))
                       (XLam (flattenT (XArray ?? t1)) (raiseX x2)).

 | XApp x1 x2  => XApp (XProj 1 (vectX x1)) (vect X2).

use implicit lifting context.
 always keep current context on stack d
*)

(* Weak normal forms. *)
Inductive wnfX : exp -> Prop :=
 | Wnf_XVar 
   : forall i
   , wnfX (XVar i)

 | Wnf_XLamT
   : forall x1
   , wnfX (XLamT x1)

 | Wnf_XLamR
   : forall x1
   , wnfX (XLamR x1)

 | Wnf_XLam
   : forall t1 x2
   , wnfX (XLam t1 x2).

Hint Constructors wnfX.



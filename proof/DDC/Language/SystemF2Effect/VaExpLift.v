
Require Export DDC.Language.SystemF2Effect.VaExpBase.
Require Export DDC.Base.
Require Import Coq.Logic.FunctionalExtensionality.


(********************************************************************)
(* Lift type indices in expressions. *)
Fixpoint liftTX (d: nat) (xx: exp) : exp :=
  match xx with
  |  XVar _     => xx
  |  XLoc _     => xx

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

  |  XUpdate c i ts x1 x2
  => XUpdate c i (map (liftTT 1 d) ts) (liftTX d x1) (liftTX d x2)

 end

 with liftTA (d: nat) (aa: alt) : alt :=
  match aa with
  |  AAlt dc xx
  => AAlt dc (liftTX d xx)
  end.
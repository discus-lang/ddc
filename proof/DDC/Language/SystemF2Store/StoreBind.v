
Require Export DDC.Language.SystemF2Store.StoreValue.
Require Export DDC.Language.SystemF2Store.TyJudge.


(* Store binding with a constructor tag and some storeable values *)
Inductive sbind :=
 | SObj : datacon -> list svalue -> sbind.


Definition store  := list sbind.
Hint Unfold store.


(* Store binding is well typed under some data type defs and store environment. *)
Inductive TYPEB (ds: defs) (se: stenv) : sbind -> ty -> Prop := 
 | TyObj 
   :  forall tc ks dc dcs tsFields tsParam svs xs
   ,  hasDef ds (DefType tc ks dcs)
   -> hasDef ds (DefData dc tsFields tc)
   -> Forall2 (KIND nil) tsParam ks
   -> Forall2 svalueOf   xs svs
   -> Forall2 (TYPE ds nil nil se) xs (map (substTTs 0 tsParam) tsFields)
   -> TYPEB ds se (SObj dc svs) (makeTApps (TCon tc) tsParam).
Hint Constructors TYPEB.



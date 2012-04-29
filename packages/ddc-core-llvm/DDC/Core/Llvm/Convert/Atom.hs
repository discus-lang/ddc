
module DDC.Core.Llvm.Convert.Atom
        ( mconvAtom
        , mconvAtoms
        , takeLocalV
        , takeGlobalV)
where
import DDC.Llvm.Instr
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Platform
import DDC.Core.Brine.Base.Sanitize
import qualified DDC.Core.Brine.Output  as E
import qualified DDC.Core.Exp           as C


-- Atoms ----------------------------------------------------------------------
-- | Take a variable or literal from an expression.
--   These can be used directly in instructions.
mconvAtom :: Platform -> C.Exp a E.Name -> Maybe Exp
mconvAtom pp xx
 = case xx of
        C.XVar _ (C.UName (E.NameVar n) t)
         -> let n' = sanitizeName n
            in  Just $ XVar (Var (NameLocal n') (convType pp t))

        C.XCon _ (C.UPrim (E.NameTag  tag) t)
         -> Just $ XLit (LitInt (convType pp t) tag)

        C.XCon _ (C.UPrim (E.NameNat  nat) t)
         -> Just $ XLit (LitInt (convType pp t) nat)

        C.XCon _ (C.UPrim (E.NameInt  val _) t)
         -> Just $ XLit (LitInt (convType pp t) val)

        C.XCon _ (C.UPrim (E.NameWord val _) t)
         -> Just $ XLit (LitInt (convType pp t) val)

        _ -> Nothing


-- | Convert several atoms to core.
mconvAtoms :: Platform -> [C.Exp a E.Name] -> Maybe [Exp]
mconvAtoms pp xs
        = sequence $ map (mconvAtom pp) xs


-- Utils ----------------------------------------------------------------------
-- | Take a variable from an expression as a local var, if any.
takeLocalV  :: Platform -> C.Exp a E.Name -> Maybe Var
takeLocalV pp xx
 = case xx of
        C.XVar _ (C.UName (E.NameVar str) t)
          -> Just $ Var (NameLocal str) (convType pp t)
        _ -> Nothing


-- | Take a variable from an expression as a local var, if any.
takeGlobalV  :: Platform -> C.Exp a E.Name -> Maybe Var
takeGlobalV pp xx
 = case xx of
        C.XVar _ (C.UName (E.NameVar str) t)
          -> Just $ Var (NameGlobal str) (convType pp t)
        _ -> Nothing


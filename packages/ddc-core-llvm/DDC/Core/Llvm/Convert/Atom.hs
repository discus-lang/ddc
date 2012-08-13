{-# OPTIONS -fno-warn-unused-binds -fno-warn-unused-matches #-}
module DDC.Core.Llvm.Convert.Atom
        ( mconvAtom
        , mconvAtoms
        , takeLocalV
        , takeGlobalV)
where
import DDC.Llvm.Instr
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Salt.Platform
import DDC.Type.Env                     (Env)
import qualified DDC.Type.Env           as Env
import qualified DDC.Core.Salt          as A
import qualified DDC.Core.Salt.Name     as A
import qualified DDC.Core.Exp           as C


-- Atoms ----------------------------------------------------------------------
-- | Take a variable or literal from an expression.
--   These can be used directly in instructions.
mconvAtom 
        :: Platform             -- ^ Platform specification.
        -> Env A.Name           -- ^ Kind environment.
        -> Env A.Name           -- ^ Type environment.
        -> C.Exp a A.Name       -- ^ Expression to convert.
        -> Maybe Exp

mconvAtom pp kenv tenv xx
 = case xx of

        -- Variables. Their names need to be sanitized before we write
        -- them to LLVM, as LLVM doesn't handle all the symbolic names
        -- that Disciple Core accepts.
        C.XVar _ u@(C.UName (A.NameVar n))
         |  Just t      <- Env.lookup u tenv
         -> let n'      = A.sanitizeName n
                t'      = convType pp kenv t
            in  Just $ XVar (Var (NameLocal n') t')

        -- Literals. 
        C.XCon _ (C.DaConAlgebraic n t)
         -> case n of
                A.NameNat  nat   -> Just $ XLit (LitInt (convType pp kenv t) nat)
                A.NameInt  val   -> Just $ XLit (LitInt (convType pp kenv t) val)
                A.NameWord val _ -> Just $ XLit (LitInt (convType pp kenv t) val)
                A.NameTag  tag   -> Just $ XLit (LitInt (convType pp kenv t) tag)
                _                -> Nothing

        _ -> Nothing


-- | Convert several atoms to core.
mconvAtoms 
        :: Platform             -- ^ Platform specification
        -> Env A.Name           -- ^ Kind environment.
        -> Env A.Name           -- ^ Type environment.
        -> [C.Exp a A.Name]     -- ^ Atoms to convert.
        -> Maybe [Exp]

mconvAtoms pp kenv tenv xs
        = sequence $ map (mconvAtom pp kenv tenv) xs


-- Utils ----------------------------------------------------------------------
-- | Take a variable from an expression as a local var, if any.
takeLocalV  
        :: Platform             -- ^ Platform specification.
        -> Env A.Name           -- ^ Kind environment.
        -> Env A.Name           -- ^ Type environment.
        -> C.Exp a A.Name       
        -> Maybe Var

takeLocalV pp kenv tenv xx
 = case xx of
        C.XVar _ u@(C.UName (A.NameVar str))
          |  Just t       <- Env.lookup u tenv
          -> Just $ Var (NameLocal str) (convType pp kenv t)
        _ -> Nothing


-- | Take a variable from an expression as a local var, if any.
takeGlobalV  
        :: Platform             -- ^ Platform specification.
        -> Env A.Name           -- ^ Kind environment.
        -> Env A.Name           -- ^ Type environment.
        -> C.Exp a A.Name
        -> Maybe Var

takeGlobalV pp kenv tenv xx
 = case xx of
        C.XVar _ u@(C.UName (A.NameVar str))
          |  Just t      <- Env.lookup u tenv
          -> Just $ Var (NameGlobal str) (convType pp kenv t)
        _ -> Nothing


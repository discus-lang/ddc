module DDC.Core.Llvm.Convert.Atom
        ( mconvAtom
        , mconvAtoms
        , takeLocalV
        , takeGlobalV)
where
import DDC.Llvm.Syntax
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Salt.Platform
import DDC.Type.Env                     (KindEnv, TypeEnv)
import qualified DDC.Type.Env           as Env
import qualified DDC.Core.Salt          as A
import qualified DDC.Core.Salt.Name     as A
import qualified DDC.Core.Exp           as C


-- Atoms ----------------------------------------------------------------------
-- | Take a variable or literal from an expression.
--   These can be used directly in instructions.
mconvAtom 
        :: Platform
        -> KindEnv A.Name
        -> TypeEnv A.Name
        -> C.Exp a A.Name
        -> Maybe Exp

mconvAtom pp kenv tenv xx
 = case xx of

        -- Variables. Their names need to be sanitized before we write
        -- them to LLVM, as LLVM doesn't handle all the symbolic names
        -- that Disciple Core accepts.
        C.XVar _ u@(C.UName (A.NameVar n))
         |  Just t      <- Env.lookup u tenv
         -> let n'      = A.sanitizeName n
                t'      = convertType pp kenv t
            in  Just $ XVar (Var (NameLocal n') t')

        -- Literals. 
        C.XCon _ dc
         | C.DaConPrim n t <- dc
         -> case n of
                A.NameLitBool bool  
                 -> let i | bool        = 1
                          | otherwise   = 0
                    in Just $ XLit (LitInt (convertType pp kenv t) i)

                A.NameLitNat  nat   -> Just $ XLit (LitInt (convertType pp kenv t) nat)
                A.NameLitInt  val   -> Just $ XLit (LitInt (convertType pp kenv t) val)
                A.NameLitWord val _ -> Just $ XLit (LitInt (convertType pp kenv t) val)
                A.NameLitTag  tag   -> Just $ XLit (LitInt (convertType pp kenv t) tag)
                _                   -> Nothing

        _ -> Nothing


-- | Convert several atoms to core.
mconvAtoms 
        :: Platform
        -> KindEnv A.Name
        -> TypeEnv A.Name
        -> [C.Exp a A.Name]
        -> Maybe [Exp]

mconvAtoms pp kenv tenv xs
        = sequence $ map (mconvAtom pp kenv tenv) xs


-- Utils ----------------------------------------------------------------------
-- | Take a variable from an expression as a local var, if any.
takeLocalV  
        :: Platform
        -> KindEnv A.Name
        -> TypeEnv A.Name
        -> C.Exp a A.Name       
        -> Maybe Var

takeLocalV pp kenv tenv xx
 = case xx of
        C.XVar _ u@(C.UName (A.NameVar str))
          |  Just t       <- Env.lookup u tenv
          -> Just $ Var (NameLocal str) (convertType pp kenv t)
        _ -> Nothing


-- | Take a variable from an expression as a local var, if any.
takeGlobalV  
        :: Platform
        -> KindEnv A.Name
        -> TypeEnv A.Name
        -> C.Exp a A.Name
        -> Maybe Var

takeGlobalV pp kenv tenv xx
 = case xx of
        C.XVar _ u@(C.UName (A.NameVar str))
          |  Just t      <- Env.lookup u tenv
          -> Just $ Var (NameGlobal str) (convertType pp kenv t)
        _ -> Nothing


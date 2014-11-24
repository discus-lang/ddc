
module DDC.Core.Llvm.Convert.Exp.Atom
        ( mconvAtom
        , mconvAtoms
        , takeLocalV
        , takeGlobalV)
where
import DDC.Llvm.Syntax
import DDC.Core.Llvm.Convert.Exp.Base
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Salt.Platform
import DDC.Base.Pretty
import Control.Monad
import DDC.Type.Env                             (KindEnv, TypeEnv)
import qualified DDC.Type.Env                   as Env
import qualified DDC.Core.Salt                  as A
import qualified DDC.Core.Salt.Convert          as A
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Exp                   as C


-- Atoms ----------------------------------------------------------------------
-- | Convert a variable or literal value to LLVM. 
--   These values can be used directly as arguments to LLVM instructions.
mconvAtom 
        :: Context
        -> C.Exp a A.Name
        -> Maybe Exp

mconvAtom ctx xx
 = let  pp      = contextPlatform ctx
        kenv    = contextKindEnv  ctx
        tenv    = contextTypeEnv  ctx
   in case xx of

        -- Variable names must be sanitized before we write them to LLVM,
        -- as LLVM doesn't handle all the symbolic names that Disciple Core
        -- accepts.
        C.XVar _ u@(C.UName nm)
         |  Just t     <- Env.lookup u tenv
         ,  Just n     <- A.takeNameVar nm
         -> let n'      = A.sanitizeName n
                t'      = convertType pp kenv t

            in  case takeGlobalV ctx xx of
                 Just var       -> Just $ XVar var
                 _              -> Just $ XVar (Var (NameLocal n') t')

        -- Literal unit values are represented as a null pointer.
        C.XCon _ C.DaConUnit
         -> Just $ XLit (LitNull (TPointer (tObj pp)))

        -- Primitive unboxed literals.
        C.XCon _ dc
         | C.DaConPrim n t <- dc
         -> let t'      = convertType pp kenv t
            in  case n of
                 A.NameLitBool b
                  -> let i | b           = 1
                           | otherwise   = 0
                    in Just $ XLit (LitInt t' i)

                 A.NameLitNat  nat   -> Just $ XLit (LitInt t' nat)
                 A.NameLitInt  val   -> Just $ XLit (LitInt t' val)
                 A.NameLitWord val _ -> Just $ XLit (LitInt t' val)
                 A.NameLitTag  tag   -> Just $ XLit (LitInt t' tag)
                 _                   -> Nothing

        _ -> Nothing


-- | Convert several atoms to core.
mconvAtoms :: Context -> [C.Exp a A.Name] -> Maybe [Exp]
mconvAtoms ctx xs
        = sequence $ map (mconvAtom ctx) xs


-- Utils ----------------------------------------------------------------------
-- | Take a variable from an expression as a local var, if any.
takeLocalV  
        :: Platform
        -> KindEnv A.Name  -> TypeEnv A.Name
        -> C.Exp a A.Name  -> Maybe Var

takeLocalV pp kenv tenv xx
 = case xx of
        C.XVar _ u@(C.UName nm)
          |  Just t       <- Env.lookup u tenv
          ,  Just str     <- A.takeNameVar nm
          ,  str'         <- A.sanitizeName str
          -> Just $ Var (NameLocal str') (convertType pp kenv t)
        _ -> Nothing


-- | Take a variable from an expression as a global var, if any.
takeGlobalV  
        :: Context
        -> C.Exp a A.Name  -> Maybe Var

takeGlobalV ctx xx
 = let  pp      = contextPlatform    ctx
        mm      = contextModule      ctx
        kenv    = contextKindEnvTop  ctx
        tenv    = contextTypeEnvTop  ctx
   in case xx of
        C.XVar _ u@(C.UName nSuper)
         | Just t   <- Env.lookup u tenv
         -> let  
                mImport  = lookup nSuper (C.moduleImportValues mm)
                mExport  = lookup nSuper (C.moduleExportValues mm)
                Just str = liftM renderPlain $ A.seaNameOfSuper mImport mExport nSuper

            in  Just $ Var (NameGlobal str) (convertType pp kenv t)

        _ -> Nothing        


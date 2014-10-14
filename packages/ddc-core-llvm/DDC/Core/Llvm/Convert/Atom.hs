
module DDC.Core.Llvm.Convert.Atom
        ( mconvAtom
        , mconvAtoms
        , takeLocalV
        , takeGlobalV)
where
import DDC.Llvm.Syntax
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Context
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
-- | Take a variable or literal from an expression.
--   These can be used directly in instructions.
mconvAtom 
        :: Platform
        -> Context
        -> KindEnv A.Name
        -> TypeEnv A.Name
        -> C.Exp a A.Name
        -> Maybe Exp

mconvAtom pp context kenv tenv xx
 = case xx of

        -- Variables. Their names need to be sanitized before we write
        -- them to LLVM, as LLVM doesn't handle all the symbolic names
        -- that Disciple Core accepts.
        C.XVar _ u@(C.UName nm)
         |  Just t     <- Env.lookup u tenv
         ,  Just n     <- A.takeNameVar nm
         -> let n'      = A.sanitizeName n
                t'      = convertType pp kenv t

                mm      = coreModuleOfContext context
                (kenvTop, tenvTop) = topEnvOfContext context

            in  case takeGlobalV pp mm kenvTop tenvTop xx of
                 Just var       -> Just $ XVar var
                 _              -> Just $ XVar (Var (NameLocal n') t')


        -- Literals. 
        C.XCon _ C.DaConUnit
         -> Just $ XLit (LitNull (TPointer (tObj pp)))

        C.XCon _ dc
         | C.DaConPrim n t <- dc
         -> case n of
                A.NameLitBool b
                 -> let i | b           = 1
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
        -> Context
        -> KindEnv A.Name
        -> TypeEnv A.Name
        -> [C.Exp a A.Name]
        -> Maybe [Exp]

mconvAtoms pp context kenv tenv xs
        = sequence $ map (mconvAtom pp context kenv tenv) xs


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


-- | Take a variable from an expression as a local var, if any.
takeGlobalV  
        :: Platform        -> C.Module () A.Name
        -> KindEnv A.Name  -> TypeEnv A.Name
        -> C.Exp a A.Name  -> Maybe Var

takeGlobalV pp mm kenv tenv xx
 | C.XVar _ u@(C.UName nSuper)   <- xx
 , Just t   <- Env.lookup u tenv
 = let  
        mImport  = lookup nSuper (C.moduleImportValues mm)
        mExport  = lookup nSuper (C.moduleExportValues mm)
        Just str = liftM renderPlain $ A.seaNameOfSuper mImport mExport nSuper

   in   Just $ Var (NameGlobal str) (convertType pp kenv t)
        
 | otherwise
 = Nothing


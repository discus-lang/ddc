
module DDC.Core.Llvm.Convert.Exp.Atom
        ( mconvAtom
        , takeLocalV
        , takeGlobalV)
where
import DDC.Llvm.Syntax
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Context
import DDC.Core.Llvm.Convert.Base
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
-- | If this looks like an atomic expression, 
--    then if it is one then produce an computation to convert it to LLVM, 
--    otherwise Nothing.
--
--   If the atom is mistyped or malformed then running the compution will
--   throw an exception in the ConvertM monad.
--
--   Converted atoms can be used directly as arguments to LLVM instructions.
--
mconvAtom :: Context -> C.Exp a A.Name -> Maybe (ConvertM Exp)
mconvAtom ctx xx
 = let  pp      = contextPlatform ctx
        kenv    = contextKindEnv  ctx
        tenv    = contextTypeEnv  ctx
   in case xx of

        -- Global names
        -- TODO: don't we also need to sanitize these?
        C.XVar _ (C.UName _)
         |  Just mv     <- takeGlobalV ctx xx
         -> Just $ do  
                var     <- mv
                return  $ XVar var

        -- Local names
        -- Variable names must be sanitized before we write them to LLVM,
        -- as LLVM doesn't handle all the symbolic names that Disciple Core
        -- accepts.
        C.XVar _ u@(C.UName nm)
         |  Just t      <- Env.lookup u tenv
         ,  Just n      <- A.takeNameVar nm
         -> Just $ do
                let n'  = A.sanitizeName n
                t'      <- convertType pp kenv t
                return  $ XVar (Var (NameLocal n') t')

        -- Literal unit values are represented as a null pointer.
        C.XCon _ C.DaConUnit
         -> Just $ return $ XLit (LitNull (TPointer (tObj pp)))

        -- Primitive unboxed literals.
        C.XCon _ dc
         | C.DaConPrim n t <- dc
         -> do case n of
                A.NameLitBool b
                 -> let i | b           = 1
                          | otherwise   = 0
                    in Just $ do
                        t' <- convertType pp kenv t
                        return $ XLit (LitInt t' i)

                A.NameLitNat nat   
                 -> Just $ do
                        t' <- convertType pp kenv t
                        return $ XLit (LitInt t' nat)

                A.NameLitInt  val
                 -> Just $ do
                        t' <- convertType pp kenv t
                        return $ XLit (LitInt t' val)

                A.NameLitSize val
                 -> Just $ do
                        t' <- convertType pp kenv t
                        return $ XLit (LitInt t' val)

                A.NameLitWord val _
                 -> Just $ do
                        t' <- convertType pp kenv t
                        return $ XLit (LitInt t' val)

                A.NameLitFloat val _
                 -> Just $ do
                        t' <- convertType pp kenv t
                        return $ XLit (LitFloat t' val)

                A.NameLitString bs
                 -> Just $ do
                        -- Add string constant to the constants map.
                        -- These will be allocated in static memory, and given
                        -- the returned name.
                        var     <- addConstant (LitString bs)
                        
                        return  $ XGet (TPointer (TInt 8))
                                       (XVar var) [0, 0]


                A.NameLitTag  tag   
                 -> Just $ do
                        t' <- convertType pp kenv t
                        return $ XLit (LitInt t' tag)

                _ -> Nothing

        _ -> Nothing


-- Local Variables ------------------------------------------------------------
-- | Take a variable from an expression as a local var, if any.
takeLocalV  
        :: Platform
        -> KindEnv A.Name  -> TypeEnv A.Name
        -> C.Exp a A.Name  
        -> Maybe (ConvertM Var)

takeLocalV pp kenv tenv xx
 = case xx of
        C.XVar _ u@(C.UName nm)
          |  Just t     <- Env.lookup u tenv
          ,  Just str   <- A.takeNameVar nm
          ,  str'       <- A.sanitizeName str
          -> Just $ do 
                t'      <- convertType pp kenv t
                return $ Var (NameLocal str') t'

        _ ->    Nothing


-- Global Variables / Names ---------------------------------------------------
-- | Take a variable from an expression as a global var, if any.
takeGlobalV  
        :: Context
        -> C.Exp a A.Name  
        -> Maybe (ConvertM Var)

takeGlobalV ctx xx
 = let  pp      = contextPlatform    ctx
        mm      = contextModule      ctx
        kenv    = contextKindEnvTop  ctx
        tenv    = contextTypeEnvTop  ctx

   in case xx of
        C.XVar _ u@(C.UName nSuper)
         | Just t   <- Env.lookup u tenv
         -> Just $ do
                let mImport  = lookup nSuper (C.moduleImportValues mm)
                let mExport  = lookup nSuper (C.moduleExportValues mm)
                let Just str = liftM renderPlain 
                             $ A.seaNameOfSuper mImport mExport nSuper

                t'      <- convertType pp kenv t
                return  $ Var (NameGlobal str) t'

        _ ->    Nothing


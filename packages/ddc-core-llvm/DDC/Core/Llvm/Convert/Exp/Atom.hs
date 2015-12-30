
module DDC.Core.Llvm.Convert.Exp.Atom
        ( mconvArg
        , mconvAtom

        , bindLocalV,   bindLocalB,     bindLocalBs
        , takeLocalV
        , takeGlobalV)
where
import DDC.Llvm.Syntax
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Context
import DDC.Core.Llvm.Convert.Base
import DDC.Core.Salt.Platform
import DDC.Control.Monad.Check
import DDC.Base.Pretty
import Control.Monad
import qualified DDC.Type.Env                   as Env
import qualified DDC.Core.Salt                  as A
import qualified DDC.Core.Salt.Convert          as A
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Exp                   as C
import qualified Data.Map                       as Map
import qualified Data.List                      as List


-- Arguments ------------------------------------------------------------------
-- | Convert a function argument.
mconvArg :: Context -> A.Arg -> Maybe (ConvertM Exp)
mconvArg ctx aa
 = case aa of
        A.RWitness _    -> Nothing
        A.RExp x        -> mconvAtom ctx x
        A.RType _       -> Nothing


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
mconvAtom :: Context -> A.Exp -> Maybe (ConvertM Exp)
mconvAtom ctx xx
 = let  pp      = contextPlatform ctx
        kenv    = contextKindEnv  ctx
   in case xx of

        -- Global names
        A.XVar (C.UName _)
         |  Just mv     <- takeGlobalV ctx xx
         -> Just $ do  
                var     <- mv
                return  $ XVar var

        -- Local names
        A.XVar (C.UName _)
         |  Just mv     <- takeLocalV ctx xx
         -> Just $ do
                var     <- mv
                return  $ XVar var

        -- Literal unit values are represented as a null pointer.
        A.XCon C.DaConUnit
         -> Just $ return $ XLit (LitNull (TPointer (tObj pp)))

        -- Primitive unboxed literals.
        A.XCon dc
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

                A.NameLitString tx
                 -> Just $ do
                        -- Add string constant to the constants map.
                        -- These will be allocated in static memory, and given
                        -- the returned name.
                        var     <- addConstant ctx $ makeLitString tx
                        let w   = 8 * platformAddrBytes pp
                        
                        return  $ XGet (TPointer (TInt 8))
                                       (XVar var) 
                                       [ XLit (LitInt (TInt w) 0)
                                       , XLit (LitInt (TInt w) 0) ]

                A.NameLitTag  tag   
                 -> Just $ do
                        t' <- convertType pp kenv t
                        return $ XLit (LitInt t' tag)

                _ -> Nothing

        _ -> Nothing


-- Local Variables ------------------------------------------------------------
-- | Add a variable and its type to the context,
--   producing the corresponding LLVM variable name.
---
--   We need to sanitize the incoming name because it may include symbols
--   that are not valid for LLVM names. We also need to uniquify them, 
--   to avoid name clashes as the the variables in a single LLVM function
--   are all bound at the same level.
--
bindLocalS :: Context -> String -> A.Type -> ConvertM (Context, Var)
bindLocalS ctx str t
 = do   t'       <- convertType (contextPlatform ctx) (contextKindEnv ctx) t
        let str'  = A.sanitizeName str
        v        <- newUniqueNamedVar str' t'
        let name  = A.NameVar str
        let ctx'  = extendTypeEnv (C.BName name t) ctx
        let ctx'' = ctx' { contextNames = Map.insert name v (contextNames ctx') }
        return (ctx'', v)


-- | Add a variable and its type to the context,
--   producing the corresponding LLVM variable name.
---
--   We need to sanitize the incoming name because it may include symbols
--   that are not valid for LLVM names. We also need to uniquify them, 
--   to avoid name clashes as the the variables in a single LLVM function
--   are all bound at the same level.
--
bindLocalV :: Context -> A.Name -> C.Type A.Name -> ConvertM (Context, Var)
bindLocalV ctx (A.NameVar str) t
 = do   bindLocalS ctx str t
bindLocalV _ _ _
 = error "bindLocalV: no name"


-- | Like `bindLocalV`, but take the binder directly.
bindLocalB  :: Context -> A.Bind -> ConvertM (Context, Var)
bindLocalB ctx b 
 = case b of
        C.BName nm t    -> bindLocalV ctx nm t
        C.BNone t       -> bindLocalV ctx (A.NameVar "_arg") t
        C.BAnon _       -> error "bindLocalB: can't convert anon binders"


-- | Like `bindLocalV`, but take some binders directly.
bindLocalBs :: Context -> [A.Bind] -> ConvertM (Context, [Var])
bindLocalBs ctx []      = return (ctx, [])
bindLocalBs ctx (b : bs)
 = do   (ctx', v)       <- bindLocalB ctx b
        (ctx'', vs)     <- bindLocalBs ctx' bs
        return  (ctx'', v : vs)


-- | Take a variable from an expression as a local var, if any.
takeLocalV  
        :: Context -> A.Exp
        -> Maybe (ConvertM Var)

takeLocalV ctx xx
 = case xx of
        A.XVar (C.UName nm)
         |     Just v     <- Map.lookup nm (contextNames ctx)
         ->    Just (return v)
        _ ->   Nothing


-- Global Variables / Names ---------------------------------------------------
-- | Take a variable from an expression as a global var, if any.
---
--   TODO: Make sure these get sanitized.
--
takeGlobalV  
        :: Context -> A.Exp
        -> Maybe (ConvertM Var)

takeGlobalV ctx xx
 = let  pp      = contextPlatform    ctx
        mm      = contextModule      ctx
        kenv    = contextKindEnvTop  ctx
        tenv    = contextTypeEnvTop  ctx

   in case xx of
        A.XVar u@(C.UName nSuper)
         | Just t   <- Env.lookup u tenv
         -> Just $ do
                let mImport  = lookup nSuper (C.moduleImportValues mm)
                let mExport  = lookup nSuper (C.moduleExportValues mm)
                let Just str = liftM renderPlain 
                             $ A.seaNameOfSuper mImport mExport nSuper

                t'      <- convertType pp kenv t
                return  $ Var (NameGlobal str) t'

        _ ->    Nothing


---------------------------------------------------------------------------------------------------
-- | Add a static constant to the map, 
--   assigning a new variable to refer to it.
addConstant :: Context -> Lit -> ConvertM Var
addConstant ctx lit
 = do   
        -- TODO: This global name should be set as having module-local
        --       scope, but we're cruftily uniquifying it with the
        --       module name instead.
        let C.ModuleName parts = C.moduleName $ contextModule ctx
        let mname       = List.intercalate "." parts

        -- Make a new variable to name the literal constant.
        (Var (NameLocal sLit) tLit) 
                <- newUniqueNamedVar mname (typeOfLit lit)

        let nLit =  NameGlobal sLit
        let vLit =  Var nLit tLit

        s        <- get
        put     $ s { llvmConstants = Map.insert vLit lit (llvmConstants s)}

        -- Although the constant itself has type tLit, when we refer
        -- to a global name in the body of the code the reference is 
        -- has pointer type.
        let vRef = Var nLit (TPointer tLit)
        return vRef



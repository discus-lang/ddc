
-- | 
module DDC.Core.Sea.Output.Check
        ( Error(..)
        , checkModule)
where
import DDC.Core.Sea.Output.Error
import DDC.Core.Sea.Output.Name
import DDC.Core.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.Check.Monad             (result, throw)
import qualified DDC.Type.Check.Monad   as G


-- | Fragment checker monad.
--   Used to manage errors.
type CheckM a = G.CheckM (Error a)


-- Module ---------------------------------------------------------------------
-- | Check for Sea language fragment violiations in a module.
--  
--   This checker only accepts core language expressions that can be mapped
--   directly onto the C language. 
--
checkModule :: Module a Name -> Maybe (Error a)
checkModule mm  
 = case (result $ checkModuleM mm) of
        Left err        -> Just err
        _               -> Nothing


-- | Fragment check a Sea module.
checkModuleM :: Module a Name -> CheckM a ()
checkModuleM mm@ModuleCore{}
        | [LRec bxs]    <- moduleLets mm
        = mapM_ checkFunctionM $ map snd bxs

        | otherwise
        = throw $ ErrorNoTopLevelLetrec mm


-- Function -------------------------------------------------------------------
-- | Fragment check a function definition.
checkFunctionM :: Exp a Name -> CheckM a ()
checkFunctionM xx
 = case xx of
        XLam _ _ x      -> checkFunctionM x
        XApp{}          -> checkBodyM xx
        XLet _ LLet{} _ -> checkBodyM xx
        XCase{}         -> checkBodyM xx

        _ -> throw $ ErrorFunctionInvalid xx


-- Body -----------------------------------------------------------------------
-- | Fragment check a function body.
checkBodyM :: Exp a Name -> CheckM a ()
checkBodyM xx
 = case xx of
        -- Function body must eventually pass control.
        XApp{}
         -> case takeXPrimApps xx of
             Just (NamePrim p, xs)
              | isControlPrim p      -> mapM_ checkArgM xs
             _                       -> throw $ ErrorBodyMustPassControl xx

        -- Variable assignment.
        XLet _ (LLet LetStrict (BName _ _) x1) x2          
         -> do  checkRValueM x1
                checkBodyM   x2

        -- Non-binding statements are only permitted with Void# return types.
        XLet _ (LLet LetStrict (BNone t) x1) x2
         | TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConVoid) _)) <- t
         -> do  checkRValueM x1
                checkBodyM   x2

        -- Case expression.
        XCase _ x alts
         -> do  checkArgM x
                mapM_ checkAltM alts

        _ -> throw $ ErrorBodyInvalid xx


-- | Check whether this primop passes control.
isControlPrim :: Prim -> Bool
isControlPrim pp
 = case pp of
        PrimControl _   -> True
        _               -> False


-- RValue -----------------------------------------------------------------------
-- | Fragment check an RValue
checkRValueM :: Exp a Name -> CheckM a ()
checkRValueM xx
 = case xx of
        -- Named variables.
        XVar _ (UName _ _)      -> return ()

        -- Literals.
        XCon _ (UPrim _ _)      -> return ()

        -- Primop and function applications.
        XApp{}
         -> case takeXPrimApps xx of
             Just (NamePrim (PrimOp    _), xs)  -> mapM_ checkArgM xs
             Just (NamePrim (PrimStore _), xs)  -> mapM_ checkArgM xs
             _ -> case takeXApps xx of
                   XVar{} :  xs                 -> mapM_ checkArgM xs
                   _                            -> throw $ ErrorRValueInvalid xx

        _ -> throw $ ErrorRValueInvalid xx


-- Arg ------------------------------------------------------------------------
-- | Fragment check a function or primop arg.
checkArgM :: Exp a Name -> CheckM a ()
checkArgM xx
 = case xx of
        -- Named variables and literals.
        XVar{}          -> return ()
        XCon{}          -> return ()

        -- Use of primops in arguments.
        XApp{}
         -> case takeXPrimApps xx of
             Just (NamePrim p, xs)
              | isArgPrim p     -> mapM_ checkArgM xs
              | otherwise       -> throw $ ErrorArgInvalid xx
             _ -> case takeXApps xx of
                   XVar{} : xs  -> mapM_ checkArgM xs
                   _            -> throw $ ErrorArgInvalid xx

        -- Type arguments.
        XType{}         -> return ()

        _ -> throw $ ErrorArgInvalid xx


-- | Check whether this primop can be used in a function argument.
isArgPrim :: Prim -> Bool
isArgPrim pp
 = case pp of
        PrimOp{}        -> True
        PrimCast{}      -> True
        PrimStore{}     -> True
        _               -> False


-- | Fragment check a case-alternative.
checkAltM :: Alt a Name -> CheckM a ()
checkAltM aa
 = case aa of
        AAlt _ x        -> checkBodyM x


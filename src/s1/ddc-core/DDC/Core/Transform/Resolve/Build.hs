
module DDC.Core.Transform.Resolve.Build where
import DDC.Core.Transform.Resolve.Context
import DDC.Core.Transform.Resolve.Base
import DDC.Type.Transform.Instantiate
import DDC.Type.Transform.SubstituteT
import DDC.Type.Transform.Unify
import DDC.Core.Exp.Annot
import DDC.Core.Codec.Text.Pretty
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Interface.Oracle      as Oracle
import qualified DDC.Core.Interface.Store       as Store


-------------------------------------------------------------------------------
-- | Try to build a term of the given type, out of the terms available
--   in the context. If in doing so we use values in imported modules
--   then also return their import specifications. These can then be
--   added as imports to the enclosing module.
--
--   TODO: Fail if multiple bindings would match.
        --
buildFromContext
        :: (Ord n, Pretty n, Show n)
        => a -> Context n -> Type n
        -> IO (Maybe (Exp a n, [ImportValue n (Type n)]))

buildFromContext !a !ctx !tWant
 = searchBinds (contextBinds ctx)
 where
        ---------------------------------------------------
        -- Search the groups of local bindings stack first.
        -- If that doesn't work then search the top-level namespace.
        searchBinds []
         = searchImports

        searchBinds (g : gs)
         = searchGroup g
         >>= \case
                Nothing -> searchBinds gs
                Just x  -> return $ Just x

        ---------------------------------------------------
        -- Search a single binding group.
        searchGroup []
         = return Nothing

        searchGroup ((nBind, tBind) : nts)
         = case buildFromBind a (contextEnvT ctx) tWant nBind tBind of
                Nothing            -> searchGroup nts
                Just (xFun, tsArg) -> buildArgs [] xFun tsArg

        ---------------------------------------------------
        -- Search the top-level imported things.
        searchImports
         | Just nTyCon <- resultTyConNameOfType tWant
         =   searchImportValues
         =<< Store.resolveValueByResultTyCon
                (Oracle.oracleStore $ contextOracle ctx)
                (Oracle.oracleImportedModules $ contextOracle ctx)
                nTyCon

         | otherwise
         = return Nothing

        ---------------------------------------------------
        searchImportValues []
         = return Nothing

        searchImportValues (iv : ivsRest)
         = case buildFromBind a
                  (contextEnvT ctx) tWant
                  (C.importValueModuleVar iv)
                  (C.typeOfImportValue iv)
            of  Nothing            -> searchImportValues ivsRest
                Just (xFun, tsArg) -> buildArgs [iv] xFun tsArg

        ---------------------------------------------------
        buildArgs ivs xFun tsArg
         =  ( fmap (fmap unzip . sequence)
            $ mapM (buildFromContext a ctx) tsArg)
         >>= \case
                Nothing -> return Nothing
                Just (xsArg, ivss)
                 -> return $ Just
                        ( xApps a xFun $ map (RImplicit . RTerm) xsArg
                        , ivs ++ concat ivss)


-------------------------------------------------------------------------------
-- | Try to build a value of the wanted type by instantiating the given
--   binding. If this succeeds then we might still need to resolve values
--   for its implicit parameters.
buildFromBind
        :: Ord n
        => a
        -> EnvT n       -- ^ Type environment.
        -> Type n       -- ^ Wanted type.
        -> n            -- ^ Name of binding in environment.
        -> Type n       -- ^ Type of binding.
        -> Maybe (Exp a n, [Type n])

buildFromBind a envt tWant nBind tBind
 -- The type of this binding is exactly what we wanted
 | equivT envt tWant tBind
 = Just (XVar a (UName nBind), [])

 -- See if we can instantiate to produce a value of the wanted type.
 | otherwise
 = let  -- Split off any type parameters.
        (bsParamType, _tBody)
         = case takeTForalls tBind of
                Just (bs, t)    -> (bs, t)
                Nothing         -> ([], tBind)

        -- Instantiate the type with new existentials.
        tsArgExists
         = [ TCon (TyConExists i k)
           | i <- [0..]
           | k <- map typeOfBind bsParamType]

        Just tBind_inst
         = instantiateTs tBind tsArgExists

        -- Split of any implicit value parameters.
        -- We might be able to build expressions for these separately.
        (tsParamTerm, tResult)
         = takeTFunImplicits tBind_inst

        -- Try to unify the wanted type with the instantiated result
        -- type of the current binding.
   in case unifyExistsRight envt tWant tResult of
        -- Unification succeeded with the given constraints.
        Just cs
         | Just tsArgInst
                 <- sequence $ map (\i -> Prelude.lookup i cs)
                 $  [0 .. (length bsParamType) - 1]
         -> Just ( xApps a (XVar a (UName nBind)) $ map RType tsArgInst
                 , map (substituteExistsT cs) tsParamTerm)

        -- We couldn't instantiate the result type to the wanted one.
        _ -> Nothing


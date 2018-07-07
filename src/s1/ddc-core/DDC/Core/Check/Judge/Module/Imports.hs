
module DDC.Core.Check.Judge.Module.Imports where
import DDC.Core.Check.Judge.Type.Base           (checkTypeM)
import DDC.Core.Check.Base
import DDC.Core.Interface.Store
import DDC.Core.Module
import DDC.Core.Env.EnvX                        (EnvX)
import DDC.Control.CheckIO                      (throw)
import qualified Data.Map.Strict                as Map


---------------------------------------------------------------------------------------------------
-- | Check kinds of imported types.
checkImportTypes
        :: (Ord n, Show n, Pretty n)
        => Config n
        -> EnvT   n
        -> Mode   n
        -> [(n, ImportType n (Type n))]
        -> CheckM a n [(n, ImportType n (Type n))]

checkImportTypes config env mode nisrcs
 = let
        ctx     = contextOfEnvT env

        -- Checker mode to use.
        modeCheckImportTypes
         = case mode of
                Recon   -> Recon
                _       -> Synth []

        -- Check an import definition.
        check (n, isrc)
         = do   let k      =  kindOfImportType isrc
                (k', _, _) <- checkTypeM config ctx UniverseKind k modeCheckImportTypes
                return  (n, mapKindOfImportType (const k') isrc)

        -- Pack down duplicate import definitions.
        --   We can import the same value via multiple modules,
        --   which is ok provided all instances have the same kind.
        pack !mm []
         = return $ Map.toList mm

        pack !mm ((n, isrc) : nis)
         = case Map.lookup n mm of
                Just isrc'
                 | compat isrc isrc' -> pack mm nis
                 | otherwise         -> throw $ ErrorImportDuplicate n

                Nothing              -> pack (Map.insert n isrc mm) nis

        -- Check if two import definitions with the same name are compatible.
        -- The same import definition can appear multiple times provided
        -- each instance has the same name and kind.
        compat (ImportTypeAbstract k1) (ImportTypeAbstract k2)
                = equivT env k1 k2

        compat (ImportTypeBoxed    k1) (ImportTypeBoxed    k2)
                = equivT env k1 k2

        compat _ _ = False

   in do
        -- Check all the imports individually.
        nisrcs' <- mapM check nisrcs

        -- Check that exports with the same name are compatable,
        -- and pack down duplicates.
        pack Map.empty nisrcs'


---------------------------------------------------------------------------------------------------
-- | Check types of imported capabilities.
checkImportCaps
        :: (Ord n, Show n, Pretty n)
        => Config n
        -> EnvT n
        -> Mode n
        -> [(n, ImportCap n (Type n))]
        -> CheckM a n [(n, ImportCap n (Type n))]

checkImportCaps config env mode nisrcs
 = let
        ctx     = contextOfEnvT env

        -- Checker mode to use.
        modeCheckImportCaps
         = case mode of
                Recon   -> Recon
                _       -> Check kEffect

        -- Check an import definition.
        check (n, isrc)
         = do   let t      =  typeOfImportCap isrc
                (t', k, _) <- checkTypeM config ctx UniverseSpec
                                t modeCheckImportCaps

                -- In Recon mode we need to post-check that the imported
                -- capability really has kind Effect.
                --
                -- In Check mode we pass down the expected kind,
                -- so this is checked locally.
                --
                when (not $ isEffectKind k)
                 $ throw $ ErrorImportCapNotEffect n

                return (n, mapTypeOfImportCap (const t') isrc)

        -- Pack down duplicate import definitions.
        --   We can import the same capability via multiple modules,
        --   which is ok provided all instances have the same type.
        pack !mm []
         = return $ Map.toList mm

        pack !mm ((n, isrc) : nis)
         = case Map.lookup n mm of
                Just isrc'
                 | compat isrc isrc'    -> pack mm nis
                 | otherwise            -> throw $ ErrorImportDuplicate n

                Nothing                 -> pack (Map.insert n isrc mm) nis

        -- Check if two imported capabilities of the same name are compatiable.
        -- The same import definition can appear multiple times provided each
        -- instance has the same name and type.
        compat (ImportCapAbstract t1) (ImportCapAbstract t2)
                = equivT (contextEnvT ctx) t1 t2

    in do
        -- Check all the imports individually.
        nisrcs' <- mapM check nisrcs

        -- Check that imports with the same name are compatable,
        -- and pack down duplicates.
        pack Map.empty nisrcs'


---------------------------------------------------------------------------------------------------
-- | Check types of imported values.
checkImportValues
        :: (Ord n, Show n, Pretty n)
        => Config n
        -> EnvX n
        -> Mode n
        -> [(n, ImportValue n (Type n))]
        -> CheckM a n [(n, ImportValue n (Type n))]

checkImportValues config env mode nisrcs
 = let
        ctx = contextOfEnvX env

        -- Checker mode to use.
        modeCheckImportTypes
         = case mode of
                Recon   -> Recon
                _       -> Check kData

        -- Check an import definition.
        check (n, isrc)
         = do   let t      =  typeOfImportValue isrc
                (t', k, _) <- checkTypeM config ctx UniverseSpec
                                         t modeCheckImportTypes

                -- In Recon mode we need to post-check that the imported
                -- value really has kind Data.
                --
                -- In Check mode we pass down the expected kind,
                -- so this is checked locally.
                --
                when (not $ isDataKind k)
                 $ throw $ ErrorImportValueNotData n

                return  (n, mapTypeOfImportValue (const t') isrc)

        -- Pack down duplicate import definitions.
        --   We can import the same value via multiple modules,
        --   which is ok provided all instances have the same type.
        pack !mm []
         = return $ Map.toList mm

        pack !mm ((n, isrc) : nis)
         = case Map.lookup n mm of
                Just isrc'
                  | compat isrc isrc'   -> pack mm nis
                  | otherwise           -> throw $ ErrorImportDuplicate n

                Nothing                 -> pack (Map.insert n isrc mm) nis

        -- Check if two imported values of the same name are compatable.
        compat (ImportValueModule _ _ t1 a1)
               (ImportValueModule _ _ t2 a2)
         = equivT (contextEnvT ctx) t1 t2 && a1 == a2

        compat (ImportValueSea _ _ _ t1)
               (ImportValueSea _ _ _ t2)
         = equivT (contextEnvT ctx) t1 t2

        compat _ _ = False

   in do
        -- Check all the imports individually.
        nisrcs' <- mapM check nisrcs

        -- Check that imports with the same name are compatable,
        -- and pack down duplicates.
        pack Map.empty nisrcs'


module DDC.Core.Module.Import
        ( -- * Imported types
          ImportType    (..)
        , kindOfImportType
        , mapKindOfImportType

          -- * Imported capabilities
        , ImportCap (..)
        , typeOfImportCap
        , mapTypeOfImportCap

          -- * Imported values
        , ImportValue   (..)
        , typeOfImportValue
        , mapTypeOfImportValue)
where
import DDC.Core.Module.Name
import DDC.Type.Exp
import Control.DeepSeq


-- ImportType -------------------------------------------------------------------------------------
-- | Define a foreign type being imported into a module.
data ImportType n
        -- | Type imported abstractly.
        --
        --   Used for phantom types of kind Data, as well as regions, effects,
        --   and any other type that does not have kind Data. When a type is
        --   imported abstractly it has no associated values, so we can just
        --   say that we have the type without worrying about how to represent
        --   its associated values.
        --
        = ImportTypeAbstract
        { importTypeAbstractType      :: !(Kind n) }

        -- | Type of some boxed data.
        --
        --   The objects follow the standard heap object layout, but the code
        --   that constructs and destructs them may have been written in a 
        --   different language.
        --
        --   This is used when importing data types defined in Salt modules.
        --
        | ImportTypeBoxed
        { importTypeBoxed             :: !(Kind n) }
        deriving Show


instance NFData n => NFData (ImportType n) where
 rnf is
  = case is of
        ImportTypeAbstract k            -> rnf k
        ImportTypeBoxed    k            -> rnf k


-- | Take the kind of an `ImportType`.
kindOfImportType :: ImportType n -> Kind n
kindOfImportType src
 = case src of
        ImportTypeAbstract k            -> k
        ImportTypeBoxed    k            -> k


-- | Apply a function to the kind of an `ImportType`
mapKindOfImportType :: (Kind n -> Kind n) -> ImportType n -> ImportType n
mapKindOfImportType f isrc
 = case isrc of
        ImportTypeAbstract k            -> ImportTypeAbstract (f k)
        ImportTypeBoxed    k            -> ImportTypeBoxed    (f k)


-- ImportCapability -------------------------------------------------------------------------------
-- | Define a foreign capability being imported into a module.
data ImportCap n
        -- | Capability imported abstractly.
        --   For capabilities like (Read r) for some top-level region r
        --   we can just say that we have the capability.
        = ImportCapAbstract
        { importCapAbstractType  :: !(Type n) }
        deriving Show


instance NFData n => NFData (ImportCap n) where
 rnf ii
  = case ii of
        ImportCapAbstract t     -> rnf t


-- | Take the type of an `ImportCap`.
typeOfImportCap :: ImportCap n -> Type n
typeOfImportCap ii
 = case ii of
        ImportCapAbstract t     -> t


-- | Apply a function to the type in an `ImportCapability`.
mapTypeOfImportCap :: (Type n -> Type n) -> ImportCap n -> ImportCap n
mapTypeOfImportCap f ii
 = case ii of
        ImportCapAbstract t     -> ImportCapAbstract (f t)


-- ImportValue ------------------------------------------------------------------------------------
-- | Define a foreign value being imported into a module.
data ImportValue n
        -- | Value imported from a module that we compiled ourselves.
        = ImportValueModule
        { -- | Name of the module that we're importing from.
          importValueModuleName        :: !ModuleName 

          -- | Name of the the value that we're importing.
        , importValueModuleVar         :: !n 

          -- | Type of the value that we're importing.
        , importValueModuleType        :: !(Type n)

          -- | Calling convention for this value,
          --   including the number of type parameters, value parameters, and boxings.
        , importValueModuleArity       :: !(Maybe (Int, Int, Int)) }

        -- | Value imported via the C calling convention.
        | ImportValueSea
        { -- | Name of the symbol being imported. 
          --   This can be different from the name that we give it in the core language.
          importValueSeaVar            :: !String 

          -- | Type of the value that we're importing.
        , importValueSeaType           :: !(Type n) }
        deriving Show


instance NFData n => NFData (ImportValue n) where
 rnf is
  = case is of
        ImportValueModule mn n t mAV 
         -> rnf mn `seq` rnf n `seq` rnf t `seq` rnf mAV

        ImportValueSea v t
         -> rnf v  `seq` rnf t


-- | Take the type of an imported thing.
typeOfImportValue :: ImportValue n -> Type n
typeOfImportValue src
 = case src of
        ImportValueModule _ _ t _       -> t
        ImportValueSea      _ t         -> t


-- | Apply a function to the type in an ImportValue.
mapTypeOfImportValue :: (Type n -> Type n) -> ImportValue n -> ImportValue n
mapTypeOfImportValue f isrc
 = case isrc of
        ImportValueModule mn n t a      -> ImportValueModule mn n (f t) a
        ImportValueSea s t              -> ImportValueSea s (f t)



module DDC.Core.Module.Import
        ( ImportType    (..)
        , kindOfImportType
        , mapKindOfImportType

        , ImportValue   (..)
        , typeOfImportValue
        , mapTypeOfImportValue)
where
import DDC.Core.Module.Name
import DDC.Type.Exp
import Control.DeepSeq


-- ImportType -------------------------------------------------------------------------------------
-- | Source of some imported type.
data ImportType n
        -- | A type imported abstractly.
        --   Used for phantom types of kind Data, 
        --   as well as any type that does not have kind Data.
        = ImportTypeAbstract
        { importTypeAbstractType      :: !(Kind n) }

        -- | The type of some boxed data which is defined somewhere else.
        --   The objects follow the standard heap object layout, but the code
        --   that constructs and destructs them may have been written in a 
        --   different language.
        --   Used when importing data types defined in Salt modules.
        | ImportTypeBoxed
        { importTypeBoxed             :: !(Kind n) }
        deriving Show


instance NFData n => NFData (ImportType n) where
 rnf is
  = case is of
        ImportTypeAbstract k            -> rnf k
        ImportTypeBoxed    k            -> rnf k


-- | Take the type of an imported thing.
kindOfImportType :: ImportType n -> Kind n
kindOfImportType src
 = case src of
        ImportTypeAbstract k            -> k
        ImportTypeBoxed    k            -> k


-- | Apply a function to the kind in an ImportType.
mapKindOfImportType :: (Kind n -> Kind n) -> ImportType n -> ImportType n
mapKindOfImportType f isrc
 = case isrc of
        ImportTypeAbstract k            -> ImportTypeAbstract (f k)
        ImportTypeBoxed    k            -> ImportTypeBoxed    (f k)


-- ImportValue ------------------------------------------------------------------------------------
-- | Source of some imported value.
data ImportValue n
        -- | Value imported from a Disciple module that we compiled ourself.
        = ImportValueModule
        { importValueModuleName        :: !ModuleName 
        , importValueModuleVar         :: !n 
        , importValueModuleType        :: !(Type n)

          -- | Number of type parameters, value parameters,
          --   and boxings for top-level super.
        , importValueModuleArity       :: !(Maybe (Int, Int, Int)) }

        -- | Something imported via the C calling convention.
        | ImportValueSea
        { importValueSeaVar            :: !String 
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


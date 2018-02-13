
module DDC.Core.Discus
        ( -- * Language profile
          profile

          -- * Program Lexing
        , lexModuleString
        , lexExpString

          -- * Checking
        , checkModule

          -- * Conversion
        , saltOfDiscusModule

          -- * Names
        , Name          (..)
        , TyConDiscus    (..)
        , DaConDiscus    (..)
        , OpFun         (..)
        , OpVector      (..)
        , OpError       (..)
        , PrimTyCon     (..),   pprPrimTyConStem
        , PrimArith     (..)
        , PrimCast      (..)

          -- * Name Parsing
        , readName
        , readTyConDiscus
        , readDaConDiscus
        , readOpFun
        , readOpVectorFlag
        , readOpErrorFlag
        , readPrimTyCon,        readPrimTyConStem
        , readPrimArithFlag
        , readPrimCastFlag

        -- * Name Generation
        , freshT
        , freshX

        -- * Errors
        , Error(..))

where
import DDC.Core.Discus.Prim
import DDC.Core.Discus.Profile
import DDC.Core.Discus.Convert   hiding (Error(..))
import DDC.Core.Discus.Check
import DDC.Core.Discus.Error

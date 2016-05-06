
module DDC.Type.Exp.Simple
        ( -------------------------------------------------
          -- * Abstract Syntax
          -- ** Types
          Type          (..)
        , TypeSum       (..)
        , TyConHash     (..)
        , TypeSumVarCon (..)

          -- ** Binding
        , Binder        (..)
        , Bind          (..)
        , Bound         (..)

          -- ** Constructors
        , TyCon         (..)
        , SoCon         (..)
        , KiCon         (..)
        , TwCon         (..)
        , TcCon         (..)

          -- ** Aliases
        , Sort, Kind
        , Region, Effect, Closure


          -------------------------------------------------
          -- * Predicates
        , -- ** Binders
          isBNone
        , isBAnon
        , isBName

          -- ** Atoms
        , isTVar
        , isBot
        , isAtomT
        , isTExists

          -- ** Kinds
        , isDataKind
        , isRegionKind
        , isEffectKind
        , isClosureKind
        , isWitnessKind

          -- ** Data Types
        , isAlgDataType
        , isWitnessType
        , isConstWitType
        , isMutableWitType
        , isDistinctWitType

          -- ** Effect Types
        , isReadEffect
        , isWriteEffect
        , isAllocEffect
        , isSomeReadEffect
        , isSomeWriteEffect
        , isSomeAllocEffect


        ---------------------------------------------------
          -- * Compounds
          -- ** Binds
        , takeNameOfBind
        , typeOfBind
        , replaceTypeOfBind
        
          -- ** Binders
        , binderOfBind
        , makeBindFromBinder
        , partitionBindsByType
        
          -- ** Bounds
        , takeNameOfBound
        , takeTypeOfBound
        , boundMatchesBind
        , namedBoundMatchesBind
        , takeSubstBoundOfBind
        , takeSubstBoundsOfBinds
        , replaceTypeOfBound

          -- ** Sorts
        , sComp, sProp

          -- ** Kinds
        , kData, kRegion, kEffect, kClosure, kWitness
        , kFun
        , kFuns
        , takeKFun
        , takeKFuns
        , takeKFuns'
        , takeResultKind

         -- ** Quantifiers
        , tForall,  tForall'
        , tForalls, tForalls'
        , takeTForalls,  eraseTForalls

          -- ** Sums
        , tBot
        , tSum

          -- ** Applications
        , tApp,          ($:)
        , tApps,         takeTApps
        , takeTyConApps
        , takePrimTyConApps
        , takeDataTyConApps
        , takePrimeRegion

          -- ** Functions
        , tFun
        , tFunOfList
        , tFunOfParamResult
        , takeTFun
        , takeTFunArgResult
        , takeTFunWitArgResult
        , takeTFunAllArgResult
        , arityOfType
        , dataArityOfType

          -- ** Suspensions
        , tSusp
        , takeTSusp
        , takeTSusps

          -- ** Implications
        , tImpl

          -- ** Units
        , tUnit

          -- ** Variables
        , tIx
        , takeTExists

          -- ** Effect types
        , tRead,        tDeepRead,      tHeadRead
        , tWrite,       tDeepWrite
        , tAlloc,       tDeepAlloc

          -- ** Witness types
        , tPure
        , tConst,       tDeepConst
        , tMutable,     tDeepMutable
        , tDistinct
        , tConData0,    tConData1)
where
import DDC.Type.Exp.Simple.Exp
import DDC.Type.Exp.Simple.NFData       ()
import DDC.Type.Exp.Simple.Pretty       ()
import DDC.Type.Exp.Simple.Predicates
import DDC.Type.Exp.Simple.Compounds




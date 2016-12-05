
module DDC.Core.Exp.Annot
        ( 
         ---------------------------------------
         -- * Abstract Syntax
          module DDC.Type.Exp

         -- ** Expressions
        , Exp           (..)
        , Param         (..)
        , Arg           (..)
        , Prim          (..)
        , Lets          (..)
        , Alt           (..)
        , Pat           (..)
        , Cast          (..)
        , pattern XLam
        , pattern XLAM

          -- ** Witnesses
        , Witness       (..)

          -- ** Data Constructors
        , DaCon         (..)

          -- ** Witness Constructors
        , WiCon         (..)

          ---------------------------------------
          -- * Predicates
        , module DDC.Type.Exp.Simple.Predicates

          -- ** Atoms
        , isXVar,  isXCon
        , isAtomX, isAtomW

          -- ** Lambdas
        , isXLAM, isXLam
        , isLambdaX

          -- ** Applications
        , isXApp

          -- ** Arguments
        , isRType
        , isRWitness
        , isRTerm
        , isRImplicit

          -- ** Cast
        , isXCast
        , isXCastBox
        , isXCastRun

          -- ** Let bindings
        , isXLet

          -- ** Patterns
        , isPDefault

          ---------------------------------------
          -- * Compounds
        , module DDC.Type.Exp.Simple.Compounds

          -- ** Annotations
        , annotOfExp
        , mapAnnotOfExp

          -- ** Lambdas
        , xLAMs
        , xLams
        , makeXLamFlags
        , takeXLAMs
        , takeXLams
        , takeXLamFlags

        , ParamTVB(..)
        , takeXLamParamTVB

          -- ** Applications
        , xApps
        , makeXAppsWithAnnots
        , takeXApps
        , takeXApps1
        , takeXAppsAsList
        , takeXAppsWithAnnots
        , takeXConApps
        , takeXPrimApps
        , takeXFragApps

          -- ** Arguments
        , takeRType
        , takeRTerm
        , takeRWitness
        , takeRImplicit
        , takeExpFromArg

          -- ** Lets
        , xLets
        , xLetsAnnot
        , splitXLets
        , splitXLetsAnnot
        , bindsOfLets
        , specBindsOfLets
        , valwitBindsOfLets

          -- ** Alternatives
        , patOfAlt
        , takeCtorNameOfAlt

          -- ** Patterns
        , bindsOfPat

          -- ** Casts
        , makeRuns

          -- ** Witnesses
        , wApp
        , wApps
        , annotOfWitness
        , takeWAppsAsList
        , takePrimWiConApps

          -- ** Data Constructors
        , xUnit, dcUnit
        , takeNameOfDaCon
        , takeTypeOfDaCon)
where
import DDC.Core.Exp.Annot.Exp
import DDC.Core.Exp.Annot.NFData                ()
import DDC.Core.Exp.Annot.Compounds
import DDC.Core.Exp.Annot.Predicates
import DDC.Type.Exp.Simple.Compounds
import DDC.Type.Exp.Simple.Predicates
import DDC.Type.Exp

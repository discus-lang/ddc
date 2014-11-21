
module DDC.Core.Tetra.Convert.Exp.Ctor
        (convertCtorApp)
where
import DDC.Core.Tetra.Convert.Data
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Tetra.Convert.Exp.Base
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Check                    (AnTEC(..))
import qualified DDC.Core.Tetra.Prim     as E
import qualified DDC.Core.Salt.Runtime   as A
import qualified DDC.Core.Salt.Name      as A
import qualified DDC.Core.Salt.Compounds as A

import DDC.Type.DataDef

import DDC.Control.Monad.Check           (throw)
import qualified Data.Map                as Map


-- | Convert a data constructor application to Salt.
convertCtorApp
        :: Show a
        => Context
        -> AnTEC a  E.Name                -- ^ Annot from deconstructed app node.
        -> DaCon    E.Name                -- ^ Data constructor being applied.
        -> [Exp (AnTEC a E.Name) E.Name]  -- ^ Data constructor arguments.
        -> ConvertM a (Exp a A.Name)

convertCtorApp ctx (AnTEC tResult _ _ a) dc xsArgsAll
 -- Handle the unit constructor.
 | DaConUnit     <- dc
 = do    return  $ A.xAllocBoxed a A.rTop 0 (A.xNat a 0)

 -- Construct algebraic data.
 | Just nCtor    <- takeNameOfDaCon dc
 , Just ctorDef  <- Map.lookup nCtor $ dataDefsCtors (contextDataDefs ctx)
 , Just dataDef  <- Map.lookup (dataCtorTypeName ctorDef) 
                 $  dataDefsTypes (contextDataDefs ctx)
 = do   
        let pp           = contextPlatform ctx
        let kenv         = contextKindEnv  ctx
        let tenv         = contextTypeEnv  ctx
        let convertX     = contextConvertExp ctx
        let tctx         = typeContext ctx

        -- Get the prime region variable.
        -- The prime region holds the outermost constructor of the object.
        trPrime          <- saltPrimeRegionOfDataType kenv tResult

        -- Split the constructor arguments into the type and value args.
        let xsArgsTypes  = [x | x@XType{} <- xsArgsAll]
        let xsArgsValues = drop (length xsArgsTypes) xsArgsAll

        -- Convert all the constructor arguments to Salt.
        xsArgsValues'    <- mapM (convertX ExpArg ctx) 
                         $  xsArgsValues

        -- Determine the Salt type for each of the arguments.
        tsArgsValues'    <- mapM (convertValueT tctx) 
                         $  map  (annotType . annotOfExp) xsArgsValues

        constructData pp kenv tenv a
                dataDef ctorDef
                trPrime xsArgsValues' tsArgsValues'


-- If this fails then the provided constructor args list is probably malformed.
-- This shouldn't happen in type-checked code.
convertCtorApp _ _ _ _
        = throw $ ErrorMalformed "Invalid constructor application."


module DDC.Core.Tetra.Convert.Exp.Ctor
        (convertCtorApp)
where
import DDC.Core.Tetra.Convert.Data
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Tetra.Convert.Exp.Base
import DDC.Core.Tetra.Convert.Exp.Lit
import DDC.Core.Pretty
import DDC.Core.Exp.Annot
import DDC.Core.Check                    (AnTEC(..))
import qualified DDC.Core.Tetra.Prim     as E
import qualified DDC.Core.Salt.Runtime   as A
import qualified DDC.Core.Salt.Name      as A
import qualified DDC.Core.Salt.Compounds as A

import DDC.Type.DataDef

import DDC.Control.Check                (throw)
import qualified Data.Map                as Map


-- | Convert a data constructor application to Salt.
convertCtorApp
        :: Show a
        => Context a
        -> AnTEC a  E.Name                -- ^ Annot from deconstructed app node.
        -> DaCon    E.Name (Type E.Name)  -- ^ Data constructor being applied.
        -> [Arg (AnTEC a E.Name) E.Name]  -- ^ Data constructor arguments.
        -> ConvertM a (Exp a A.Name)

convertCtorApp ctx (AnTEC tResult _ _ a) dc asArgsAll

 -- The unit constructor.
 | DaConUnit      <- dc
 = do    return  $ A.xAllocBoxed a A.rTop 0 (A.xNat a 0)


 -- Literal values
 | DaConPrim n _  <- dc
 , E.isNameLitUnboxed n
 =      convertLitCtor a dc


 -- Applications of the record constructor.
 --   These must be fully applied.
 | DaConRecord ns <- dc
 , tsArgsValues   <- [t | RType t <- asArgsAll]
 , xsArgsValues   <- [x | RTerm x <- drop (length tsArgsValues) asArgsAll]
 , arity          <- length ns
 , length tsArgsValues == arity
 , length xsArgsValues == arity
 = do
        let pp           = contextPlatform   ctx
        let convertX     = contextConvertExp ctx
        let tctx         = typeContext       ctx

        -- Convert all the constructor arguments to Salt.
        xsArgsValues'   <- mapM (convertX ExpArg ctx)
                        $  xsArgsValues

        -- Determine the Salt type for each of the arguments.

        tsArgsValues'   <- mapM (convertDataT tctx)
                        $  map  (annotType . annotOfExp) xsArgsValues

        -- ISSUE #433: Refactor constructData to take only the fields it uses.
        -- We can't make a real CtorDef for records because they don't have real
        -- fragment specific names. However, the constructData fn is not using
        -- the name field, so we shouldn't have to supply this bogus info.
        let ctorDef     = DataCtor
                        { dataCtorName          = E.NameCon "Record"    -- bogus name.
                        , dataCtorTag           = 0
                        , dataCtorFieldTypes    = tsArgsValues
                        , dataCtorResultType    = tResult
                        , dataCtorTypeName      = E.NameCon "Record"    -- bogus name.
                        , dataCtorTypeParams    = [BAnon t | t <- tsArgsValues] }

        constructData pp a
                ctorDef A.rTop
                xsArgsValues' tsArgsValues'


 -- Construct algebraic data.
 | Just nCtor    <- takeNameOfDaCon dc
 , Just ctorDef  <- Map.lookup nCtor $ dataDefsCtors (contextDataDefs ctx)
 , tsArgsTypes   <- [t | RType t <- asArgsAll]
 , xsArgsValues  <- [x | RTerm x <- drop (length tsArgsTypes) asArgsAll]
 , arity         <- length (dataCtorFieldTypes ctorDef)
 , length xsArgsValues == arity
 = do
        let pp          = contextPlatform ctx
        let convertX    = contextConvertExp ctx
        let tctx        = typeContext ctx

        -- Convert all the constructor arguments to Salt.
        xsArgsValues'    <- mapM (convertX ExpArg ctx)
                         $  xsArgsValues

        -- Determine the Salt type for each of the arguments.
        tsArgsValues'    <- mapM (convertDataT tctx)
                         $  map  (annotType . annotOfExp) xsArgsValues

        constructData pp a
                ctorDef A.rTop
                xsArgsValues' tsArgsValues'


-- If this fails then the provided constructor args list is probably malformed.
-- This shouldn't happen in type-checked code.
convertCtorApp _ _ dc xsArgsAll
 = throw $  ErrorMalformed
         $  "Invalid constructor application "
         ++ (renderIndent $ ppr (dc, xsArgsAll))



module DDC.Core.Tetra.Convert.Exp.PrimArith
        (convertPrimArith)
where
import DDC.Core.Tetra.Convert.Exp.Base
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Boxing
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Pretty
import DDC.Core.Exp.Annot
import DDC.Core.Check                   (AnTEC(..))
import DDC.Control.Check                (throw)
import qualified DDC.Core.Tetra.Prim    as E
import qualified DDC.Core.Salt.Name     as A


-- | Convert a Tetra arithmetic or logic primop to Salt.
convertPrimArith
        :: ExpContext                   -- ^ The surrounding expression context.
        -> Context a                    -- ^ Types and values in the environment.
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> Maybe (ConvertM a (Exp a A.Name))

convertPrimArith _ectx ctx xx
 = let  downPrimArgX = convertPrimArgX    ctx ExpArg
        downArgX     = convertX           ExpArg ctx 
        convertX     = contextConvertExp  ctx
   in case xx of

        ---------------------------------------------------
        -- Saturated application of a primitive operator.
        XApp a xa xb
         | (x1, xsArgs)               <- takeXApps1 xa xb
         , XVar _ (UPrim nPrim tPrim) <- x1

         -- All the value arguments have representatable types.
         , all isSomeRepType
                $  map (annotType . annotOfExp)
                $  filter (not . isXType) xsArgs

         -- The result is representable.
         , isSomeRepType (annotType a)

         -> Just $ if -- Check that the primop is saturated.
             length xsArgs == arityOfType tPrim
             then do
                x1'     <- downArgX x1
                xsArgs' <- mapM downPrimArgX xsArgs
                
                case nPrim of
                 E.NamePrimArith o False
                  |  elem o [ E.PrimArithEq, E.PrimArithNeq
                            , E.PrimArithGt, E.PrimArithLt
                            , E.PrimArithLe, E.PrimArithGe ]
                  ,  [t1, z1, z2] <- xsArgs'
                  ->  return $ xApps (annotTail a) x1' [t1, z1, z2]

                 _ -> return $ xApps (annotTail a) x1' xsArgs'

             else throw $ ErrorUnsupported xx
                   $ text "Partial application of primitive operators is not supported."

        ---------------------------------------------------
        -- This isn't an arithmetic or logic primop.
        _ -> Nothing


-- | Although we ditch type arguments when applied to general functions,
--   we need to convert the ones applied directly to primops, 
--   as the primops are specified polytypically.
convertPrimArgX 
        :: Context a
        -> ExpContext                   -- ^ What context we're converting in.
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> ConvertM a (Exp a A.Name)

convertPrimArgX ctx ectx xx
 = let  convertX = contextConvertExp ctx
   in case xx of
        XType a t
         -> do  t'      <- convertDataPrimitiveT t
                return  $ XType (annotTail a) t'

        XWitness{}
         -> throw $ ErrorUnsupported xx
                  $ text "Witness expressions are not part of the Tetra language."

        _ -> convertX ectx ctx xx



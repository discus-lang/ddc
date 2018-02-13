
module DDC.Core.Discus.Convert.Exp.PrimArith
        (convertPrimArith)
where
import DDC.Core.Discus.Convert.Exp.Base
import DDC.Core.Discus.Convert.Type
import DDC.Core.Discus.Convert.Boxing
import DDC.Core.Discus.Convert.Error
import DDC.Core.Pretty
import DDC.Core.Exp.Annot
import Data.Maybe
import DDC.Core.Check                   (AnTEC(..))
import DDC.Control.Check                (throw)
import qualified DDC.Core.Discus.Prim    as E
import qualified DDC.Core.Salt.Name     as A


-- | Convert a Discus arithmetic or logic primop to Salt.
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
         | (x1, asArgs)               <- takeXApps1 xa xb
         , XVar _ (UPrim nPrim tPrim) <- x1

         -- All the value arguments have representatable types.
         , all isSomeRepType
                $  map (annotType . annotOfExp)
                $  mapMaybe takeExpFromArg asArgs

         -- The result is representable.
         , isSomeRepType (annotType a)

         -> Just $ if -- Check that the primop is saturated.
             length asArgs == arityOfType tPrim
             then do
                x1'     <- downArgX x1
                asArgs' <- mapM downPrimArgX asArgs

                case nPrim of
                 E.NamePrimArith o False
                  |  elem o [ E.PrimArithEq, E.PrimArithNeq
                            , E.PrimArithGt, E.PrimArithLt
                            , E.PrimArithLe, E.PrimArithGe ]
                  ,  [t1, z1, z2] <- asArgs'
                  ->  return $ xApps (annotTail a) x1' [t1, z1, z2]

                 _ -> return $ xApps (annotTail a) x1' asArgs'

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
        -> Arg (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> ConvertM a (Arg a A.Name)

convertPrimArgX ctx ectx aa
 = let  convertX = contextConvertExp ctx
   in case aa of
        RType t
         -> do  t'      <- convertDataPrimitiveT t
                return  $ RType t'

        RWitness{}
         -> throw $ ErrorUnsupportedArg aa
                  $ text "Witness expressions are not part of the Discus language."

        RTerm x
         -> do  x'      <- convertX ectx ctx x
                return  $ RTerm x'

        RImplicit arg
         -> do  arg'    <- convertPrimArgX ctx ectx arg
                return  $ RImplicit arg'




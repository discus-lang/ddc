
module DDC.Core.Discus.Convert.Exp.PrimRecord
        (convertPrimRecord)
where
import DDC.Core.Discus.Convert.Exp.Base
import DDC.Core.Discus.Convert.Error
import DDC.Core.Exp.Annot
import DDC.Core.Check                           (AnTEC(..))
import qualified DDC.Core.Discus.Prim            as E
import qualified DDC.Core.Salt.Runtime          as A
import qualified DDC.Core.Salt.Name             as A


convertPrimRecord
        :: ExpContext                   -- ^ The surrounding expression context.
        -> Context a                    -- ^ Types and values in environment.
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> Maybe (ConvertM a (Exp a (A.Name)))

convertPrimRecord _ectx ctx xxExp
 = let convertX = contextConvertExp ctx
   in  case xxExp of

        -- Project a single field from a record.
        XApp a _ _
         -- Unpack the application of the projection primitive.
         | Just ( PProject  nField
                , [RType tRecord, RType _tField, RTerm xRecord])
                <- takeXPrimApps xxExp

         -- Get the list of fields from the record that we are projecting from.
         , Just (TyConSpec (TcConRecord nsFields),   _tsFields)
                <- takeTyConApps tRecord

         -- Lookup the index and type of the field we want from the record.
         , Just (iField    :: Integer)
                 <- lookup nField $ zip nsFields [0..]

         -> Just $ do
                let a'  =  annotTail a

                -- Convert the record expression
                xRecord' <- convertX ExpArg ctx xRecord

                -- Project out the field that we want.
                return   $  A.xGetFieldOfBoxed a' A.rTop A.rTop
                                        xRecord' iField

        _ -> Nothing

module DDC.Core.Discus.Convert.Exp.PrimRecord
        (convertPrimRecord)
where
import DDC.Core.Discus.Convert.Exp.Base
import DDC.Core.Discus.Convert.Error
import DDC.Core.Exp.Annot
import DDC.Data.Label
import DDC.Core.Check                           (AnTEC(..))
import qualified DDC.Core.Discus.Prim           as E
import qualified DDC.Core.Salt.Compounds        as A
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
        XApp a _ _

         -- Record construction -----------------------------------------------
         | Just (PRecord ls, rs) <- takeXPrimApps xxExp
         , _tsField <- [t | RType t <- rs]
         , xsField  <- [x | RTerm x <- rs]
         -> Just $ do
                let a' = annotTail a

                -- Info table index is set to 0 as records are already self describing.
                let xInfoIdx    = A.xWord a' 0 32

                -- Use 8 byte hashes for each field name.
                let xLengthRaw  = A.xNat a' $ fromIntegral $ length ls * 8

                -- Store all the field pointers in the object.
                let xLengthPtrs = A.xNat a' $ fromIntegral $ length ls

                xsField' <- mapM (convertX ExpArg ctx) xsField

                return
                   -- allocate the object.
                 $ XLet a' (LLet  (BAnon (A.tPtr A.rTop A.tObj))
                                  (A.xAllocMixed a' A.rTop xInfoIdx xLengthRaw xLengthPtrs))

                   -- get a pointer to the raw payload.
                 $ XLet a' (LLet  (BAnon (A.tPtr A.rTop (A.tWord 64)))
                                  (A.xCastPtr a' A.rTop (A.tWord 64) (A.tWord 8)
                                    (A.xPayloadOfMixed a' A.rTop (XVar a' (UIx 0)))))

                 $ foldr (XLet a') (XVar a' (UIx 1))

                    -- write field hashes
                 $  [ LLet (BNone A.tVoid)
                           (A.xPoke a' A.rTop (A.tWord 64)
                                (A.xPlusPtr a' A.rTop (A.tWord 64)
                                        (XVar a' (UIx 0)) (A.xNat a' (ix * 8)))
                                (A.xWord a' (fromIntegral $ hashOfLabel l) 64))
                    | ix     <- [0..]
                    | l      <- ls ]

                    -- write field values
                 ++ [ LLet (BNone A.tVoid)
                           (A.xSetFieldOfMixed a' A.rTop A.rTop (XVar a' (UIx 1)) ix xField)
                    | ix     <- [0..]
                    | xField <- xsField' ]

         -- Record field projection --------------------------------------------
         |  Just (PProject l, [RTerm xRecord]) <- takeXPrimApps xxExp
         -> Just $ do
                let a' = annotTail a

                xRecord' <- convertX ExpArg ctx xRecord

                return $ A.xRecordProject a'
                                A.rTop A.rTop xRecord'
                                (A.xWord a' (fromIntegral (hashOfLabel l)) 64)

        _ -> Nothing


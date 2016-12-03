
module DDC.Core.Transform.Reannotate
        (Reannotate (..))
where
import DDC.Core.Module
import DDC.Core.Exp.Annot.Exp
import Control.Monad.Identity


-- | Apply the given function to every annotation in a core thing.
class Reannotate c where
 reannotate  :: (a -> b) -> c a n -> c b n
 reannotate f xx
  = runIdentity (reannotateM (\x -> return $ f x) xx)

 reannotateM :: forall m a b n. Monad m 
             => (a -> m b) -> c a n -> m (c b n)


instance Reannotate Module where
 reannotateM f
     (ModuleCore name isHeader
                 exportKinds   exportTypes 
                 importKinds   importCaps   importTypes  importDataDefs importTypeDefs
                 dataDefsLocal typeDefsLocal
                 body)

  = do  body'   <- reannotateM f body
        return  $  ModuleCore name isHeader
                        exportKinds  exportTypes
                        importKinds  importCaps   importTypes  importDataDefs importTypeDefs
                        dataDefsLocal typeDefsLocal
                        body'


instance Reannotate Exp where
 reannotateM f xx
  = let down x   = reannotateM f x
    in case xx of
        XVar     a u            -> XVar     <$> f a <*> pure u
        XPrim    a p            -> XPrim    <$> f a <*> pure p
        XCon     a u            -> XCon     <$> f a <*> pure u
        XAbs     a b x          -> XAbs     <$> f a <*> pure b <*> down x
        XApp     a x1 x2        -> XApp     <$> f a            <*> down x1  <*> down x2
        XLet     a lts x        -> XLet     <$> f a            <*> down lts <*> down x
        XCase    a x alts       -> XCase    <$> f a            <*> down x   <*> mapM down alts
        XCast    a c x          -> XCast    <$> f a            <*> down c   <*> down x
        XType    a t            -> XType    <$> f a <*> pure t
        XWitness a w            -> XWitness <$> f a <*> down w


instance Reannotate Lets where
 reannotateM f xx
  = let down x  = reannotateM f x
    in case xx of
        LLet b x
         -> LLet <$> pure b <*> down x

        LRec bxs 
         -> do  let (bs, xs) = unzip bxs
                xs'     <- mapM down xs
                return  $ LRec $ zip bs xs'

        LPrivate b t bs
         -> return $ LPrivate b t bs


instance Reannotate Alt where
 reannotateM f aa
  = case aa of
        AAlt w x                -> AAlt w <$> reannotateM f x


instance Reannotate Cast where
 reannotateM f cc
  = let down x  = reannotateM f x
    in case cc of
        CastWeakenEffect eff    -> pure $ CastWeakenEffect eff
        CastPurify w            -> CastPurify <$> down w
        CastBox                 -> pure CastBox
        CastRun                 -> pure CastRun


instance Reannotate Witness where
 reannotateM f ww
  = let down x = reannotateM f x
    in case ww of
        WVar  a u               -> WVar  <$> f a <*> pure u
        WCon  a c               -> WCon  <$> f a <*> pure c
        WApp  a w1 w2           -> WApp  <$> f a <*> down w1 <*> down w2
        WType a t               -> WType <$> f a <*> pure t


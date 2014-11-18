
module DDC.Core.Flow.Transform.Slurp.Resize
    ( Resize(..)
    , slurpResize
    , lookupOrDie
    , seqEitherMaybe )
where
import DDC.Core.Flow.Transform.Slurp.Error
import DDC.Core.Flow.Prim
import DDC.Core.Compounds.Simple
import DDC.Core.Exp.Simple
import Control.Applicative
import qualified Data.Map               as Map

data Resize
 = Id    Name
 | AppL  Name Name
 | AppR  Name Name
 | App   Name Name Name Name Resize Resize 
 | Sel1  Name Name Name      Resize
 | Segd  Name Name Name      Resize
 | Cross Name Name Name      Resize

slurpResize
    :: Map.Map Name Resize
    -> Exp ()  Name
    -> Either Error (Maybe Resize)

slurpResize rs xx

 | Just ( NameOpSeries OpSeriesResizeId
        , [ _, tK ] )
                <- takeXPrimApps xx
 = return (Id <$> nameOfType tK)

 | Just ( NameOpSeries OpSeriesResizeAppL
        , [ _, tK, tL ] )
                <- takeXPrimApps xx
 = return (AppL <$> nameOfType tK <*> nameOfType tL)

 | Just ( NameOpSeries OpSeriesResizeAppR
        , [ _, tK, tL ] )
                <- takeXPrimApps xx
 = return (AppR         <$> nameOfType tK <*> nameOfType tL)

 | Just ( NameOpSeries OpSeriesResizeApp
        , [ _, tK, tK', tL, tL'
          , XVar (UName rL)
          , XVar (UName rR) ] )
                <- takeXPrimApps xx
 = do   rL'     <- lookupOrDie rL rs
        rR'     <- lookupOrDie rR rs
        return (App     <$> nameOfType tK <*> nameOfType tK'
                        <*> nameOfType tL <*> nameOfType tL'
                        <*> Just rL'      <*> Just rR')

 | Just ( NameOpSeries OpSeriesResizeSel1
        , [ _, tJ, tK, tL
          , _
          , XVar (UName r) ] )
                <- takeXPrimApps xx
 = do   r'      <- lookupOrDie r rs
        return (Sel1    <$> nameOfType tJ <*> nameOfType tK
                        <*> nameOfType tL
                        <*> Just r')

 | Just ( NameOpSeries OpSeriesResizeSegd
        , [ _, tJ, tK, tL
          , _
          , XVar (UName r) ] )
                <- takeXPrimApps xx
 = do   r'      <- lookupOrDie r rs
        return (Segd    <$> nameOfType tJ <*> nameOfType tK
                        <*> nameOfType tL
                        <*> Just r')


 | Just ( NameOpSeries OpSeriesResizeCross
        , [ _, tJ, tK, tL
          , _
          , XVar (UName r) ] )
                <- takeXPrimApps xx
 = do   r'      <- lookupOrDie r rs
        return (Cross   <$> nameOfType tJ <*> nameOfType tK
                        <*> nameOfType tL
                        <*> Just r')



 | otherwise
 = return Nothing


nameOfType :: Exp () Name -> Maybe Name

nameOfType (XType (TVar (UName n)))
 = Just n

nameOfType _
 = Nothing


lookupOrDie
    :: Name
    -> Map.Map Name v
    -> Either Error v
lookupOrDie n m
 = case Map.lookup n m of
    Just v  -> return v
    Nothing -> Left $ ErrorNotInContext n

seqEitherMaybe :: Either a (Maybe b) -> Maybe (Either a b)
seqEitherMaybe x
 = case x of
   Left a         -> Just (Left a)
   Right Nothing  -> Nothing
   Right (Just b) -> Just (Right b)


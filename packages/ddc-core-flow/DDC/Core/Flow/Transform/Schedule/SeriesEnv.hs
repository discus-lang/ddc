
module DDC.Core.Flow.Transform.Schedule.SeriesEnv
        ( SeriesEnv (..)
        , emptySeriesEnv
        , insertElemForSeries

        , bindNextElem
        , bindNextElems
        
        , elemBindOfSeriesBind
        , elemBoundOfSeriesBound
        , elemNameOfSeriesName
        , elemTypeOfSeriesType
        , rateTypeOfSeriesType )
where
import DDC.Core.Flow.Transform.Schedule.Nest
import DDC.Core.Flow.Exp.Procedure
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Compounds
import DDC.Core.Exp
import qualified Data.Map       as Map
import Data.Map                 (Map)


-------------------------------------------------------------------------------
data SeriesEnv
        = SeriesEnv
        { -- | Maps the bound for a whole series to the bound for
          --   a single element in the series context. 
          envSeriesElems        :: Map Name (Bound Name) 
        }


-- | An empty series environment.
emptySeriesEnv :: SeriesEnv
emptySeriesEnv
        = SeriesEnv Map.empty


-- | Insert an entry into the series environment.
insertElemForSeries
        :: Name -> Bound Name -> SeriesEnv -> SeriesEnv
insertElemForSeries
 n u (SeriesEnv env)
        = SeriesEnv (Map.insert n u env)


-- | Produce the `Bound` that holds the next element for the given series,
--   which exists in the series's context.
--
--   We first try to look up the required bound from the series environment,
--   if it's not already available then insert a statement into the loop nest
--   to get actually get the next element from the series.
bindNextElem 
        :: Name                 -- ^ Name of series.
        -> Type Name            -- ^ Rate of series
        -> Type Name            -- ^ Series element type.
        -> SeriesEnv            -- ^ Current series environment.
        -> [Loop]               -- ^ Current loop nest.
        -> (Bound Name, SeriesEnv, [Loop])

bindNextElem nSeries tRate tElem env nest0
        -- There is already a mapping in the environment.
        | Just uElem    <- Map.lookup nSeries (envSeriesElems env)
        = (uElem, env, nest0)
        
        -- Insert a statement into the loop nest to get the next element
        -- from the series.
        | otherwise
        = let   -- bound for the single element
                Just nElem      = elemNameOfSeriesName nSeries
                uElem           = UName nElem

                -- Expression to get the next element from the series.
                uSeries = UName nSeries
                uIndex  = UIx 0
                xGet    = xNext tRate tElem (XVar () uSeries) (XVar () uIndex)

                -- Insert the statement into the loop nest.
                nest1   = insertBody nest0 (Context tRate)
                                [ BodyStmt (BName nElem tElem) xGet ]
                                           
                env'    = env { envSeriesElems 
                                        = Map.insert nSeries uElem 
                                                    (envSeriesElems env) }
           
           in   (uElem, env', nest1)


-- | Like `bindNextElem`, but handle several series at once.
bindNextElems 
        :: [(Name, Type Name, Type Name)] 
                                -- ^ Names, rates, and element types.
        -> SeriesEnv            -- ^ Current series environment.
        -> [Loop]               -- ^ Current loop nest.
        -> ([Bound Name], SeriesEnv, [Loop])

bindNextElems junk env nest0
 = case junk of
        []      
         -> ([], env, nest0)
        
        (nSeries, tRate, tElem) : junk'
         -> let (uElem1,  env1, nest1)  
                        = bindNextElem  nSeries tRate tElem env nest0
                
                (uElems', env', nest')
                        = bindNextElems junk' env1 nest1
            
            in  (uElem1 : uElems', env', nest')


-- | Given the bind of a series,  produce the bound that refers to the
--   next element of the series in its context.
elemBindOfSeriesBind   :: Bind Name  -> Maybe (Bind Name)
elemBindOfSeriesBind bSeries
        | BName nSeries tSeries' <- bSeries
        , Just nElem    <- elemNameOfSeriesName nSeries
        , Just tElem    <- elemTypeOfSeriesType tSeries'
        = Just $ BName nElem tElem

        | otherwise
        = Nothing
 

-- | Given the bound of a series, produce the bound that refers to the
--   next element of the series in its context.
elemBoundOfSeriesBound :: Bound Name -> Maybe (Bound Name)
elemBoundOfSeriesBound uSeries
        | UName nSeries <- uSeries
        , Just nElem    <- elemNameOfSeriesName nSeries
        = Just $ UName nElem

        | otherwise
        = Nothing


-- | Given the name of a whole series, 
--   produce the name that refers to the next element in its associated context.
elemNameOfSeriesName   :: Name -> Maybe Name
elemNameOfSeriesName n
 = case n of
        NameVar str     -> Just $ NameVar (str ++ "__elem")
        _               -> Nothing


-- | Given the type of a series like @Series k e@, produce the type
--   of a single element, namely the @e@.
elemTypeOfSeriesType :: Type Name -> Maybe (Type Name)
elemTypeOfSeriesType tSeries'
        | Just (_tcSeries, [_tK, tE]) <- takeTyConApps tSeries'
        = Just tE

        | otherwise
        = Nothing


-- | Given the type of a series like @Series k e@, produce the type
--   of the rate, namely the @k@.
rateTypeOfSeriesType :: Type Name -> Maybe (Type Name)
rateTypeOfSeriesType tSeries'
        | Just (_tcSeries, [tK, _tE]) <- takeTyConApps tSeries'
        = Just tK

        | otherwise
        = Nothing


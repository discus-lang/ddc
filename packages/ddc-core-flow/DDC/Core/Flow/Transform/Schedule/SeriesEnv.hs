
module DDC.Core.Flow.Transform.Schedule.SeriesEnv
        ( SeriesEnv (..)
        , emptySeriesEnv
        , insertElemForSeries

        , bindNextElem
        , bindNextElems
        
        , elemBindOfSeriesBind
        , elemBoundOfSeriesBound
        , elemTypeOfSeriesType
        , rateTypeOfSeriesType )
where
import DDC.Core.Flow.Transform.Schedule.Nest
import DDC.Core.Flow.Procedure
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp
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
        :: Name -> BoundF -> SeriesEnv -> SeriesEnv

insertElemForSeries n u (SeriesEnv env)
        = SeriesEnv (Map.insert n u env)


-- | Produce the `Bound` that holds the next element for the given series,
--   which exists in the series's context.
--
--   We first try to look up the required bound from the series environment,
--   if it's not already available then insert a statement into the loop nest
--   to get actually get the next element from the series.
bindNextElem 
        :: Name                 -- ^ Name of series.
        -> TypeF                -- ^ Rate of series
        -> TypeF                -- ^ Series element type.
        -> SeriesEnv            -- ^ Current series environment.
        -> Nest                 -- ^ Current loop nest.
        -> (BoundF, SeriesEnv, Nest)

bindNextElem nSeries tRate tElem env nest0
        -- There is already a mapping in the environment.
        | Just uElem    <- Map.lookup nSeries (envSeriesElems env)
        = (uElem, env, nest0)
        
        -- Insert a statement into the loop nest to get the next element
        -- from the series.
        | otherwise
        = let   -- bound for the single element
                nElem   = NameVarMod nSeries "elem"
                uElem   = UName nElem

                -- Expression to get the next element from the series.
                uSeries = UName nSeries
                uIndex  = UIx 0
                xGet    = xNext tRate tElem (XVar uSeries) (XVar uIndex)

                -- Insert the statement into the loop nest.
                Just nest1   
                        = (insertBody nest0 (ContextRate tRate)
                                [ BodyStmt (BName nElem tElem) xGet ])
                                           
                env'    = env { envSeriesElems 
                                        = Map.insert nSeries uElem 
                                                    (envSeriesElems env) }
           
           in   (uElem, env', nest1)


-- | Like `bindNextElem`, but handle several series at once.
bindNextElems 
        :: [(Name, TypeF, TypeF)] 
                                -- ^ Names, rates, and element types.
        -> SeriesEnv            -- ^ Current series environment.
        -> Nest                 -- ^ Current loop nest.
        -> ([BoundF], SeriesEnv, Nest)

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
elemBindOfSeriesBind   :: BindF  -> Maybe BindF
elemBindOfSeriesBind bSeries
        | BName nSeries tSeries' <- bSeries
        , nElem         <- NameVarMod nSeries "elem"
        , Just tElem    <- elemTypeOfSeriesType tSeries'
        = Just $ BName nElem tElem

        | otherwise
        = Nothing
 

-- | Given the bound of a series, produce the bound that refers to the
--   next element of the series in its context.
elemBoundOfSeriesBound :: BoundF -> Maybe BoundF
elemBoundOfSeriesBound uSeries
        | UName nSeries <- uSeries
        , nElem         <- NameVarMod nSeries "elem"
        = Just $ UName nElem

        | otherwise
        = Nothing


-- | Given the type of a series like @Series k e@, produce the type
--   of a single element, namely the @e@.
elemTypeOfSeriesType :: TypeF -> Maybe TypeF
elemTypeOfSeriesType tSeries'
        | Just (_tcSeries, [_tK, tE]) <- takeTyConApps tSeries'
        = Just tE

        | otherwise
        = Nothing


-- | Given the type of a series like @Series k e@, produce the type
--   of the rate, namely the @k@.
rateTypeOfSeriesType :: TypeF -> Maybe TypeF
rateTypeOfSeriesType tSeries'
        | Just (_tcSeries, [tK, _tE]) <- takeTyConApps tSeries'
        = Just tK

        | otherwise
        = Nothing


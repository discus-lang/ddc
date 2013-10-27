module DDC.Core.Flow.Transform.Rates.SeriesOfVector
        (seriesOfVectorModule
        ,seriesOfVectorFunction)
where
import DDC.Core.Collect.Free
import DDC.Core.Collect.Free.Simple ()
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Transform.Rates.Constraints
import DDC.Core.Flow.Transform.Rates.Fail
import DDC.Core.Flow.Transform.Rates.Graph
import DDC.Core.Module
import DDC.Core.Transform.Annotate
import DDC.Core.Transform.Deannotate
import qualified DDC.Type.Env           as Env

import           Control.Applicative
import           Control.Monad
import           Data.List              (intersect, nub)
import qualified Data.Map               as Map
import           Data.Maybe             (catMaybes)
import qualified Data.Set               as Set

import DDC.Core.Pretty (ppr)
import Debug.Trace

seriesOfVectorModule :: ModuleF -> (ModuleF, [(Name,Fail)])
seriesOfVectorModule mm
 = let body       = deannotate (const Nothing)
                  $ moduleBody mm

       (lets, xx) = splitXLets body
       letsErrs   = map seriesOfVectorLets lets

       lets'      = map       fst letsErrs
       errs       = concatMap snd letsErrs

       body'      = annotate ()
                  $ xLets lets' xx


   in  trace ("ORIGINAL:"++ show (ppr $ moduleBody mm))
     $ trace ("MODULE:" ++ show (ppr body'))
       (mm { moduleBody = body' }, errs)
       


seriesOfVectorLets :: LetsF -> (LetsF, [(Name,Fail)])
seriesOfVectorLets ll
 | LLet b@(BName n _) x <- ll
 , (x',errs)  <- seriesOfVectorFunction x
 = (LLet b x', map (\f -> (n,f)) errs)

 | LRec bxs             <- ll
 , (bs,xs)              <- unzip bxs
 , (xs',_errs)          <- unzip $ map seriesOfVectorFunction xs
 = (LRec (bs `zip` xs'), []) -- TODO errors

 | otherwise
 = (ll, [])


-- | Takes a single function body. Function body must be in a-normal form.
seriesOfVectorFunction :: ExpF -> (ExpF, [Fail])
seriesOfVectorFunction fun
 = run $ do
        -- Peel off the lambdas
        let (lams, body)   = takeXLamFlags_safe fun
        -- TODO: Check that it's a-normal form
            (lets, xx)     = splitXLets         body
        -- Split into name and values and warn for recursive bindings
        binds             <- takeLets           lets
        let tymap          = takeTypes          (concatMap valwitBindsOfLets lets ++ map snd lams)

        -- TODO check that binds are only vector primitives,
        -- OR   if not vector primitives, do not refer to bound vectors

        let names = map fst binds
        -- Make sure names are unique
        when (length names /= length (nub names)) $
          warn FailNamesNotUnique

        (_constrs, equivs)
                  <- checkBindConstraints binds

        let extras = catMaybes
                   $ map (takeNameOfBind . snd) lams
        let graph  = graphOfBinds         binds extras

        let rets   = catMaybes
                   $ map takeNameOfBound
                   $ Set.toList
                   $ freeX Env.empty xx
        
        loops     <- schedule             graph equivs rets

        binds'    <- orderBinds           binds loops

        True <- trace ("TYMAP:" ++ show tymap) return True
        True <- trace ("NAMES,LOOPS,NAMES':" ++ show (names, loops, map (map fst) binds')) return True

        let outputs = map lOutputs loops
        let inputs  = map lInputs  loops

        return $ construct lams (zip3 binds' outputs inputs) equivs tymap xx

-- | Peel the lambdas off, or const if there are none
takeXLamFlags_safe x
 | Just (binds, body) <- takeXLamFlags x
 = (binds, body)
 | otherwise
 = ([],    x)


-- | Split into name and values and warn for recursive bindings
takeLets :: [LetsF] -> LogFailures [(Name, ExpF)]
takeLets lets
 = concat <$> mapM get lets
 where
  get (LLet (BName n _) x) = return [(n,x)]
  get (LLet (BNone _)   _) = return []
  get (LLet (BAnon _)   _) = w      FailNoDeBruijnAllowed
  get (LRec        _     ) = w      FailRecursiveBindings
  get (LLetRegions _ _   ) = w      FailLetRegionNotHandled
  get (LWithRegion _     ) = w      FailLetRegionNotHandled

  w err                    = warn err >> return []

-- | Split into name and values and warn for recursive bindings
takeTypes :: [Bind Name] -> Map.Map Name TypeF
takeTypes binds
 = Map.fromList $ concatMap get binds
 where
  get (BName n t) = [(n,t)]
  get _           = []


data Loop
 = Loop 
 { lBindings :: [Name]
 , lOutputs  :: [Name]
 , lInputs   :: [Name]
 } deriving (Eq,Show)

schedule :: Graph -> EquivClass -> [Name] -> LogFailures [Loop]
schedule graph equivs rets
 = let type_order    = map (canonName equivs . Set.findMin) equivs
       -- minimumBy length $ map scheduleTypes $ permutations type_order
       (wts, graph') = scheduleTypes graph equivs type_order
       loops         = scheduleAll (map snd wts) graph'
       -- Use the original graph to find vars that cross loop boundaries
       outputs       = scheduleOutputs loops graph rets
       inputs        = scheduleInputs  loops graph
   in  trace ("GRAPH,GRAPH',WTS,EQUIVS:" ++ show (graph, graph', wts, equivs)) return $ zipWith3 Loop loops outputs inputs

scheduleTypes :: Graph -> EquivClass -> [Name] -> ([(Name, Map.Map Name Int)], Graph)
scheduleTypes graph types type_order
 = foldl go ([],graph) type_order
 where
  go (w,g) ty
   = let w' = typedTraversal g types ty
         g' = mergeWeights   g w'
     in  ((ty,w') : w, g')


scheduleAll :: [Map.Map Name Int] -> Graph -> [[Name]]
scheduleAll weights graph
 = loops
 where
  weights' = map invertMap  weights
  topo     = graphTopoOrder graph
  loops    = map getNames topo

  getNames n
   = find n (weights `zip` weights')

  find _ []
   = []

  find n ((w,w') : rest)
   | Just i  <- n `Map.lookup` w
   , Just ns <- i `Map.lookup` w'
   = ns

   | otherwise
   = find n rest

-- Find any variables that cross loop boundaries - they must be reified
scheduleOutputs :: [[Name]] -> Graph -> [Name] -> [[Name]]
scheduleOutputs loops graph rets
 = map output loops
 where
  output ns
   = graphOuts ns ++ filter (`elem` ns) rets 

  graphOuts ns
   = concatMap (\(k,es) -> if   k `elem` ns
                           then []
                           else ns `intersect` map fst es)
   $ Map.toList graph

-- Find any variables that cross loop boundaries - they must be reified
scheduleInputs  :: [[Name]] -> Graph -> [[Name]]
scheduleInputs  loops graph
 = map input loops
 where
  input ns
   = filter (\n -> not (n `elem` ns))
   $ graphIns ns

  graphIns ns
   = concatMap (map fst . mlookup "graphIns" graph) ns

typedTraversal :: Graph -> EquivClass -> Name -> Map.Map Name Int
typedTraversal graph types current_type
 = restrictTypes types current_type
 $ traversal graph w
 where
  w  u v = if w' u v then 1 else 0

  w' (u, fusible) v
   | canonName types u == current_type
   = canonName types v /= current_type || fusible

   | otherwise
   = False


restrictTypes :: EquivClass -> Name -> Map.Map Name Int -> Map.Map Name Int
restrictTypes types current_type weights
 = Map.filterWithKey restrict weights
 where
  restrict n _
   = canonName types n == current_type


orderBinds :: [(Name,ExpF)] -> [Loop] -> LogFailures [[(Name,ExpF)]]
orderBinds binds loops
 = let bindsM = Map.fromList binds
       order  = map lBindings loops
       get k  | Just v <- Map.lookup k bindsM
              = [(k,v)]
              | otherwise
              = []
   in  return $ map (\o -> concatMap get o) order


construct
        :: [(Bool, BindF)]
        -> [([(Name, ExpF)], [Name], [Name])]
        -> EquivClass
        -> Map.Map Name TypeF
        -> ExpF
        -> ExpF
construct lams loops equivs tys xx
 = let body   = foldr convert xx loops
   in  makeXLamFlags lams body
 where
  convert (binds, outputs, inputs) body
   = convertToSeries binds outputs inputs equivs tys body


-- TODO still missing the join of procs,
-- split output procs into separate functions
convertToSeries :: [(Name,ExpF)] -> [Name] -> [Name] -> EquivClass -> Map.Map Name TypeF -> ExpF -> ExpF
convertToSeries binds outputs inputs equivs tys xx
 = kloked
 where
  kloked
   = Map.foldWithKey klokn wrapped klokMap

  klokn :: Name -> [(Name,TypeF)] -> ExpF -> ExpF
  klokn cn vecs body
   = let kN     = NameVarMod cn "k"
         vFlags = map (\(n,t) -> (False, BName (NameVarMod n "s") (tSeries (TVar (UName kN)) t))) vecs
     in  xApps (xVarOpSeries (OpSeriesRunSeries $ length vecs))
               (map (XType . snd) vecs
               ++ [(makeXLamFlags ([(True, BName kN kRate)] ++ vFlags) body)])

  klokMap :: Map.Map Name [(Name,TypeF)]
  klokMap = foldl collectKloks Map.empty inputs
  collectKloks m inp
   | tyI <- mlookup "collectKloks" tys inp
   , Just (_tcVec, [tyA]) <- takeTyConApps tyI
   , tyI == tVector tyA
   = Map.insertWith (++) (canonName equivs inp) [(inp, tyA)] m
   | otherwise
   = m

  wrapped 
   = foldr wrap filled binds

  wrap (n,x) body
   = wrapSeriesX equivs n (mlookup "wrap" tys n) x body

  -- fill vectors and read references
  filled
   = foldr fill xx binds

  fill (n,x) body
   | n `elem` outputs
   = fillSeriesX equivs n (mlookup "fill" tys n) x body
   | otherwise
   = body


fillSeriesX :: EquivClass -> Name -> TypeF -> ExpF -> ExpF -> ExpF
fillSeriesX equivs name ty xx wrap
 | Just (f, _args)                       <- takeXApps xx
 , XVar (UPrim (NameOpVector ov) _)     <- f
 = case ov of
   -- any folds MUST be known as outputs, so this is safe
   OpVectorReduce
    -> XLet (LLet (BName name ty) (xRead ty (XVar $ UName $ NameVarMod name "ref")))
             wrap
   _
    | [_vec, tyR]                       <- takeTApps ty
    -> let k  = canonName equivs name
           s  = NameVarMod name "s"
           op = xVarOpSeries OpSeriesFill
       in  XLet (LLet (BNone tProcess)
                 $ xApps op [XVar $ UName k, XType tyR, XVar $ UName name, XVar $ UName s])
                 wrap
   _
    ->
     wrap
 | otherwise
 = wrap


wrapSeriesX :: EquivClass -> Name -> TypeF -> ExpF -> ExpF -> ExpF
wrapSeriesX equivs name ty xx wrap
 | Just (op, args)                      <- takeXApps xx
 , XVar (UPrim (NameOpVector ov) _)     <- op
 = case ov of
   OpVectorReduce
    | [_tA, f, z, vA]   <- args
    , XVar (UName nvA)  <- vA
    , Just kA           <- klok nvA
    -> xLets [ LLet (BName (NameVarMod name "ref") (tRef ty)) (xNew ty z)
             , LLet (BName (NameVarMod name "proc") tProcess)
                   $ xApps (xVarOpSeries OpSeriesReduce)
                           [kA, XType ty, XVar (UName (NameVarMod name "ref")), f, z, modNameX "s" vA] ]
             wrap

   OpVectorMap n
    | (tys, f : rest) <- splitAt (n+1) args
    , length rest     == n
    , Just kT         <- klok name
    , Just tySer      <- tyS
    , rest'           <- map (modNameX "s") rest
    -> bind  (NameVarMod name "s") tySer
     $ xApps (xVarOpSeries (OpSeriesMap n))
             ([kT] ++ tys ++ [f] ++ rest')

   OpVectorFilter
    -> xx
   _
    -> xx
 | otherwise
 = xx

 where
  bind n t x
   = XLet (LLet (BName n t) x) wrap

  klok n
   | n'      <- canonName equivs n
   , kN      <- NameVarMod n' "k"
   = Just $ XType $ TVar $ UName kN
   | otherwise
   = Nothing

  tyS
   | [_vec, tyR]        <- takeTApps ty
   , Just (XType kS)    <- klok name
   = Just $ tSeries kS tyR
   | otherwise
   = Nothing


--  tySeries
--   | Vector n

xVarOpSeries n = XVar (UPrim (NameOpSeries n) (typeOpSeries n))

modNameX :: String -> ExpF -> ExpF
modNameX s xx
 = case xx of
    XVar (UName n)
     -> XVar (UName (NameVarMod n s))
    _
     -> xx

{-

\as,bs...
cs = map as
ds = filter as
n  = fold ds
es = map3 bs cs
return es

==>
schedule graph equivs [es]
==>

[ [ds, n]
, [cs, es] ]

-}

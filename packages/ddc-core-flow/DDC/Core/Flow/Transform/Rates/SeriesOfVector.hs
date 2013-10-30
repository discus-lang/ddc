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
       loops         = scheduleAll (map snd wts) graph graph'
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


scheduleAll :: [Map.Map Name Int] -> Graph -> Graph -> [[Name]]
scheduleAll weights graph graph'
 = loops
 where
  weights' = map invertMap  weights
  topo     = graphTopoOrder graph'
  loops    = map getNames topo

  getNames n
   = sort $ find n (weights `zip` weights')

  original_order = graphTopoOrder graph

  -- Cheesy hack to get ns in same order as the original graph's topo:
  -- filter topo to only those elements in ns
  sort ns
   = filter (flip elem ns) original_order

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
   = nub $ concatMap (map fst . mlookup "graphIns" graph) ns

typedTraversal :: Graph -> EquivClass -> Name -> Map.Map Name Int
typedTraversal graph types current_type
 = restrictTypes types current_type
 $ traversal graph w
 where
  w  u v = if w' u v then 1 else 0

  w' (u, fusible) v
   | canonName types u == current_type
   = canonName types v /= current_type || not fusible

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
 = let lets   = concatMap convert loops
   in  makeXLamFlags lams
     $ xLets lets
     $ xx
 where
  convert (binds, outputs, inputs)
   = convertToSeries binds outputs inputs equivs tys


-- TODO still missing the join of procs,
-- split output procs into separate functions
convertToSeries :: [(Name,ExpF)] -> [Name] -> [Name] -> EquivClass -> Map.Map Name TypeF -> [LetsF]
convertToSeries binds outputs inputs equivs tys
 =  concat setups
 ++ [LLet (BNone tBool) (runprocs inputs' processes)]
 ++ concat readrefs
 where
  runprocs :: [(Name,TypeF)] -> ExpF -> ExpF
  runprocs vecs@((cn,_):_) body
   = let cnn    = canonName equivs cn
         kN     = NameVarMod cnn "k"
         kFlags = [ (True,  BName kN kRate)
                  , (False, BNone $ tRateNat $ TVar $ UName kN)]
         vFlags = map (\(n,t) -> (False, BName (NameVarMod n "s") (tSeries (TVar (UName kN)) t))) vecs
     in  xApps (xVarOpSeries (OpSeriesRunProcess $ length vecs))
               (  map (XType .         snd) vecs
               ++ map (XVar  . UName . fst) vecs
               ++ [(makeXLamFlags (kFlags ++ vFlags) body)])

  -- TODO introduce rate parameter for generates?
  runprocs [] body
   = body

  inputs' :: [(Name,TypeF)]
  inputs' = concatMap filterInputs inputs

  filterInputs inp
   | tyI <- mlookup "collectKloks" tys inp
   , Just (_tcVec, [tyA]) <- takeTyConApps tyI
   , tyI == tVector tyA
   = [(inp, tyA)]
   | otherwise
   = []

  processes 
   = foldr wrap joins binds

  wrap (n,x) body
   = wrapSeriesX equivs outputs n (mlookup "wrap" tys n) x body

  -- TODO
  joins
   | not $ null outputs
   = foldl1 mkJoin
   $ map (\n -> XVar $ UName $ NameVarMod n "proc") outputs
   | otherwise
   = xUnit -- ???

  mkJoin p q
   = xApps (xVarOpSeries OpSeriesJoin) [p, q]

  -- fill vectors and read references
  (setups, readrefs)
   = unzip
   $ map setread 
   $ filter (flip elem outputs . fst) binds

  setread (n,x)
   = setreadSeriesX equivs tys n (mlookup "setread" tys n) x


setreadSeriesX :: EquivClass -> Map.Map Name TypeF -> Name -> TypeF -> ExpF -> ([LetsF], [LetsF])
setreadSeriesX equivs tys name ty xx
 | Just (f, args)                       <- takeXApps xx
 , XVar (UPrim (NameOpVector ov) _)     <- f
 = case ov of
   -- any folds MUST be known as outputs, so this is safe
   OpVectorReduce
    | [_tA, _f, z, _vA]   <- args
    -> ([ LLet (BName (nm "ref") (tRef ty)) (xNew  ty z) ]
       ,[ LLet (BName  name       ty)       (xRead ty (vr $ nm "ref"))])

   _
    | [_vec, tyR]       <- takeTApps ty
    , v                 <- canonName equivs name
    , [_vec, tyCR]      <- takeTApps $ mlookup "setreadSeriesX" tys v
    -> let vl = xApps (xVarOpVector OpVectorLength)
                      [XType tyCR, XVar $ UName v]
       in  ([ LLet (BName name $ tBot kData) $ xNewVector tyR vl ]
           ,  [])

   _
    -> ([], [])
 | otherwise
 = ([],[])
 where
  nm s = NameVarMod name s
  vr n = XVar $ UName n


wrapSeriesX :: EquivClass -> [Name] -> Name -> TypeF -> ExpF -> ExpF -> ExpF
wrapSeriesX equivs outputs name ty xx wrap
 | Just (op, args)                      <- takeXApps xx
 , XVar (UPrim (NameOpVector ov) _)     <- op
 = case ov of
   OpVectorReduce
    | [_tA, f, z, vA]   <- args
    , XVar (UName nvA)  <- vA
    , kA                <- klok nvA
    -> XLet (LLet (BName name'proc tProcess)
                 $ xApps (xVarOpSeries OpSeriesReduce)
                         [kA, XType ty, XVar (UName name'ref), f, z, modNameX "s" vA])
             wrap

   OpVectorMap n
    | (tys, f : rest) <- splitAt (n+1) args
    , length rest     == n
    , kT              <- klok name
    , rest'           <- map (modNameX "s") rest
    -> XLet (LLet (BName name's $ tBot kData)
                 $ xApps (xVarOpSeries (OpSeriesMap n))
                         ([kT] ++ tys ++ [f] ++ rest'))
             wrap'fill

   OpVectorFilter
    -> xx
   _
    -> xx
 | otherwise
 = xx

 where
  name'proc = NameVarMod name "proc"
  name'ref  = NameVarMod name "ref"
  name's    = NameVarMod name "s"

  klokT n
   = let n'  = canonName equivs n
         kN  = NameVarMod n' "k"
     in  TVar $ UName kN
  klok n
   = XType $ klokT n

  tyR
   | [_vec, tyR']        <- takeTApps ty
   = Just tyR'
   | otherwise
   = Nothing

  wrap'fill
   | name `elem` outputs
   , Just tyR' <- tyR
   = XLet (LLet (BName name'proc tProcess) $ xApps fillV [klok name, XType tyR', vr name, vr name's])
           wrap
   | otherwise
   = wrap

  fillV = xVarOpSeries OpSeriesFill

  vr n = XVar $ UName n

--  tySeries
--   | Vector n

xVarOpSeries n = XVar (UPrim (NameOpSeries n) (typeOpSeries n))
xVarOpVector n = XVar (UPrim (NameOpVector n) (typeOpVector n))

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

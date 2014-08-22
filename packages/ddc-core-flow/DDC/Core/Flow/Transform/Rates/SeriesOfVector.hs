{-# OPTIONS_GHC -Wwarn #-}
module DDC.Core.Flow.Transform.Rates.SeriesOfVector
        (seriesOfVectorModule
        ,seriesOfVectorFunction)
where
import DDC.Core.Pretty
import DDC.Core.Collect
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp                           as DDC
import DDC.Core.Flow.Transform.Rates.Combinators   as Com
import DDC.Core.Flow.Transform.Rates.CnfFromExp
import DDC.Core.Flow.Transform.Rates.Fail
import DDC.Core.Flow.Transform.Rates.Graph
import qualified DDC.Core.Flow.Transform.Rates.SizeInference as SI
import DDC.Core.Flow.Transform.Rates.Linear
import DDC.Core.Module
import DDC.Core.Transform.Annotate
import DDC.Core.Transform.Deannotate
import qualified DDC.Type.Env           as Env

import qualified Data.Map as Map
import           Data.Map   (Map)
import qualified Data.Set as Set
import           Data.Set   (Set)
import Data.Maybe (catMaybes)
import Data.Monoid (mappend)


seriesOfVectorModule :: ModuleF -> (ModuleF, [(Name,Fail)])
seriesOfVectorModule mm
 = let body       = deannotate (const Nothing)
                  $ moduleBody mm

       (lets, xx) = splitXLets body
       letsErrs   = map seriesOfVectorLets lets

       lets'      = concatMap fst letsErrs
       errs       = concatMap snd letsErrs

       body'      = annotate ()
                  $ xLets lets' xx


   in  (mm { moduleBody = body' }, errs)
       


seriesOfVectorLets :: LetsF -> ([LetsF], [(Name,Fail)])
seriesOfVectorLets ll
 | LLet b@(BName n _) x <- ll
 , (x',ls',errs)  <- seriesOfVectorFunction x
 = ( ls' ++ [LLet b x']
   , map (\f -> (n,f)) errs)

 | LRec bxs             <- ll
 , (bs,xs)              <- unzip bxs
 , (xs',ls', _errs)          <- unzip3 $ map seriesOfVectorFunction xs
 = ( concat ls' ++ [LRec (bs `zip` xs')]
   , []) 
        -- We still need to produce errors if this doesn't work.

 | otherwise
 = ([ll], [])


-- | Takes a single function body. Function body must be in a-normal form.
seriesOfVectorFunction :: ExpF -> (ExpF, [LetsF], [Fail])
seriesOfVectorFunction fun
 = case cnfOfExp fun of
   Left err
    -> (fun, [], [FailCannotConvert err])
   Right prog
    -> case SI.generate prog of
           Nothing
            -> (fun, [], [])
           Just (env,s)
            -> let g          = graphOfBinds prog env
                   tmap a b   = SI.parents prog env a b
                   clustering = solve_linear g tmap
                   clusters   = map snd $ Map.toList clustering
                   (re, ls)   = reconstruct fun prog env clusters
               in  (re, ls, [])


reconstruct
        :: ExpF
        -> Program Name Name
        -> SI.Env Name
        -> [[CName Name Name]]
        -> (ExpF, [LetsF])
reconstruct fun prog env clusters
 = (makeXLamFlags lams
   $ xLets lets' xx
   , procs)
 where

  (lams, body)   = takeXLamFlags_safe fun
  (olds, xx)     = splitXLets         body

  types          = takeTypes          (concatMap valwitBindsOfLets olds ++ map snd lams)

  lets           = concatMap convert clusters
  (lets', procs) = extractProcs lets (map snd lams)

  convert c
   = let outputs = outputsOfCluster prog c
         inputs  = inputsOfCluster prog c

         justArray (NameArray a) = [a]
         justArray _             = []

         arrIns  = concatMap justArray inputs

         -- Map over the list of all binds and find only those that we want.
         -- This way is better than mapping lookup over c, as we get them in program order.
         binds   = filter (flip elem c . cnameOfBind) (_binds prog)
         binds'  = map (\a -> (a, cnameOfBind a `elem` outputs)) binds
     in  mkLets types env arrIns binds'


-- | Extract processes out so they can be made into separate bindings
extractProcs :: [LetsF] -> [DDC.Bind Name] -> ([LetsF], [LetsF])
extractProcs lets env
 = go lets $ env
 where
  go [] _
   = ([], [])

  go (l:ls) e
   -- Actually, we know they're all LLets. Maybe it should just be (Name,ExpF)
   | LLet b x <- l
   , BName nm _ <- b
   = let this = go1 b nm x e
         rest = go ls (b : e)
     in this `mappend` rest

   | otherwise
   = ([l],[]) `mappend` go ls e

  go1 b nm x e
   | Just (op, args)                                      <- takeXApps x
   , XVar (UPrim (NameOpSeries (OpSeriesRunProcess n)) _) <- op
   , (xs, [lam])                                          <- splitAt (n*2) args

   = let fs = freeX Env.empty lam

         isMentioned b
          | Just b' <- takeSubstBoundOfBind b
          = Set.member b' fs
          | otherwise
          = False

         os = filter isMentioned e

         nm' = NameVarMod nm "process"

         x' = xApps op (xs ++ [xApps (XVar $ UName nm') (map XVar (takeSubstBoundsOfBinds os))])

         p' = makeXLamFlags (map (\o -> (False, o)) os) lam
     in ([LLet b x'], [LLet (BName nm' (tBot kData)) p'])

   | otherwise
   = ([LLet b x], [])

-- | Make "lets" for a cluster.
-- If it's external, this is trivial.
-- If not, make a runProcess# etc
mkLets :: Map Name TypeF -> SI.Env Name -> [Name] -> [(Com.Bind Name Name, Bool)] -> [LetsF]
mkLets types env arrIns bs
 | any isExt (map fst bs)
 = case bs of
    [(Ext (NameArray  b) xx _, _)] -> [LLet (BName b (types Map.! b)) xx]
    [(Ext (NameScalar b) xx _, _)] -> [LLet (BName b (types Map.! b)) xx]

    _         -> error ("ddc-core-flow:DDC.Core.Flow.Transform.Rates.SeriesOfVector impossible\n" ++
                        "an external node has been clustered with another node.\n" ++
                        "this means there must be a bug in the clustering algorithm.\n" ++
                        show bs)

 -- We *could* just return an empty list in this case, but I don't think that's a good idea.
 | [] <- bs
 =               error ("ddc-core-flow:DDC.Core.Flow.Transform.Rates.SeriesOfVector impossible\n" ++
                        "a cluster was created with no bindings.\n" ++
                        "this means there must be a bug in the clustering algorithm.\n" ++
                        show bs)

 | otherwise
 = process types env arrIns
 $ map toEither bs

 where
  isExt (Ext{}) = True
  isExt _       = False

  toEither (SBind s b, out) = ((s, Left  b), out)
  toEither (ABind a b, out) = ((a, Right b), out)
  toEither (Ext{},     _)   = error "impossible"


-- | Create a process for a cluster of array and scalar bindings.
-- No externals.
--
-- TODO: 
--  1. filter arrIns to only those that must be series; first args of gather, cross etc.
--  2. what about generate?  
--  3. lift/raise Processes to their own top-level binding
process :: Map Name TypeF
        -> SI.Env Name
        -> [Name]
        -> [((Name, Either (SBind Name Name) (ABind Name Name)), Bool)]
        -> [LetsF]
process types env arrIns bs
 = let pres  = concatMap  getPre  bs
       mid   = runProcs   (mkProcs bs)
       posts = concatMap  getPost bs
   in pres ++ [LLet (BName (NameVarMod outname "runproc") tBool) mid] ++ posts
 where
  getPre b
   -- There is no point of having a reduce that isn't returned.
   | ((s, Left (Fold _ (Seed z _) _)), _)       <- b
   = [LLet (BName (NameVarMod s "ref") $ tRef $ tyOf s) (xNew (tyOf s) z)]

   -- Returned vectors
   | ((v, Right _), True)                       <- b
   = [LLet (BName v $ tVector $ sctyOf v) (xNewVector (sctyOf v) allocSize)]

   -- Otherwise, it's not returned or we needn't allocate anything
   | _                                          <- b
   = []


  getPost b
   | ((s, Left (Fold _ (Seed _ _) _)), _)       <- b
   = [ LLet (BName s $ sctyOf s) (xRead (tyOf s) (var $ NameVarMod s "ref")) ]

   -- Ignore anything else
   | _                                          <- b
   = []

  runProcs body
   = let kFlags = [ (True,  BName klok kRate)
                  , (False, BName (NameVarMod klok "r") $ tRateNat $ TVar $ UName klok) ]
         vFlags = map (\n -> (False, BName (NameVarMod n "s") (tSeries (klokT n) (sctyOf n)))) arrIns
     in  xApps (xVarOpSeries (OpSeriesRunProcess $ length arrIns))
               (  map xsctyOf arrIns
               ++ map var    arrIns
               ++ [(makeXLamFlags (kFlags ++ vFlags) body)])


  mkProcs (b:rs)
   | ((s, Left (Fold (Fun xf _) (Seed xs _) ain)), _)   <- b
   = let rest = mkProcs rs
     in  XLet (LLet   (BName (NameVarMod s "proc") $ tProcess)
              $ xApps (xVarOpSeries OpSeriesReduce)
                      [klokX ain, xtyOf s, var (NameVarMod s "ref"), xf, xs, var (NameVarMod ain "s")])
         rest

   | ((n, Right abind), out)                            <- b
   = let rest   = mkProcs rs
         n'proc = NameVarMod n "proc"
         n's    = NameVarMod n "s"
         n'flag = NameVarMod n "flags"
         n'sel  = NameVarMod n "sel"

         llet nm t x1
                = XLet (LLet (BName nm t) x1)

         go     | out
                = llet n'proc tProcess
                ( xApps (xVarOpSeries OpSeriesFill) [klokX n, xsctyOf n, var $ n, var $ n's] )
                  rest
                | otherwise
                = rest

     in  case abind of
         MapN (Fun xf _) ains
          -> llet n's (tSeries (klokT n) $ sctyOf n)
           ( xApps (xVarOpSeries (OpSeriesMap (length ains)))
                   ([klokX n] ++ (map xsctyOf  ains) ++ [xsctyOf n, xf] ++ map (var . flip NameVarMod "s") ains) )
             go

         Filter (Fun xf _) ain
          -> llet n'flag (tSeries (klokT ain) tBool)
           ( xApps (xVarOpSeries (OpSeriesMap 1))
                   [ klokX ain, xsctyOf n, XType tBool, xf, var $ NameVarMod ain "s"] )
           $ xApps (xVarOpSeries $ OpSeriesMkSel 1)
                   [ klokX ain, var n'flag
                   ,        XLAM (BName (klokV n) kRate)
                          $ XLam (BName n'sel (tSel1 (klokT ain) (klokT n)))
                          $ llet n's (tSeries (klokT n) $ sctyOf n)
                          ( xApps (xVarOpSeries OpSeriesPack)
                                  [klokX ain, klokX n, xsctyOf n, var n'sel, var $ NameVarMod ain "s"] )
                            go ]




  mkProcs []
   -- bs cannot be empty: there's no point of an empty cluster.
   = let procs = concatMap getProc bs
     in  case procs of
         (_:_)  -> foldl1 mkJoin $ concatMap getProc bs
         []     -> error "cluster with no outputs?"

  mkJoin p q
   = xApps (xVarOpSeries OpSeriesJoin) [p, q]

  getProc ((s, Left _), _)
   = [var $ NameVarMod s "proc"]
  getProc ((a, _), True)
   = [var $ NameVarMod a "proc"]
  getProc _
   = []

     
  allocSize
   -- If there are any inputs, use the size of one of those
   | (i:_) <- arrIns
   = xApps (xVarOpVector OpVectorLength) [xsctyOf i, var i]
   -- Or if it's a generate, find the size of the generate expression.
   | (sz:_) <- concatMap findGenerateSize bs
   = var sz
   -- XXX Otherwise it must be an external, and this won't be called...
   | otherwise
   = error ("ddc-core-flow: allocSize, but no size known" ++ show arrIns ++ "\n" ++ show bs)


  -- | Find the *original* klok of the first input.
  klok
   -- If there are any inputs, use the size of one of those. They should all be the same?
   | (i:_) <- arrIns
   = klokV i
   -- Otherwise, just choose one
   | (((n, _), _) :_) <- bs
   = klokV n
   | otherwise
   = error "empty cluster!"

  klokV = getKlok env
  klokT = TVar . UName . klokV
  klokX = XType . klokT

  findGenerateSize ((_, Right (Generate sz _)), _)
   = [sz]
  findGenerateSize _
   = []


  -- We just need to find the name of any binding
  outname
   = fst $ fst $ head bs

  tyOf n  = types Map.! n
  sctyOf  = getScalarType . tyOf
  xtyOf   = XType . tyOf
  xsctyOf = XType . sctyOf

  var n   = XVar $ UName n


xVarOpSeries n = XVar (UPrim (NameOpSeries n) (typeOpSeries n))
xVarOpVector n = XVar (UPrim (NameOpVector n) (typeOpVector n))


-- | Get underlying scalar of a vector type - or just return original type if it's not a vector.
getScalarType :: TypeF -> TypeF
getScalarType tt
 = case takePrimTyConApps tt of
        Just (NameTyConFlow TyConFlowVector, [sc])      -> sc
        _                                               -> tt


-- | Create map of types of binders
takeTypes :: [DDC.Bind Name] -> Map Name TypeF
takeTypes binds
 = Map.fromList $ concatMap get binds
 where
  get (BName n t) = [(n,t)]
  get _           = []


getKlok :: SI.Env Name -> Name -> Name
getKlok e n
 | Just t <- SI.lookupV e n
 = ty $ goT t
 | otherwise
 = ty n
 where
  ty = flip NameVarMod "k"

  goT (SI.TVar kv)
   = goKV kv
  -- This doesn't matter much..
  goT (SI.TCross ta tb)
   = NameVarMod (goT ta) (show tb)

  goKV (SI.KV v)
   = v
  goKV (SI.K' kv)
   = NameVarMod (goKV kv) "'"

{-
-- We still need to join procs,
-- split output procs into separate functions
convertToSeries 
        :: (Name -> Name) -> [(Name,ExpF)] -> [Name] -> [Name] 
        -> EquivClass -> Map.Map Name TypeF -> [LetsF]

convertToSeries getMax binds outputs inputs equivs tys
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
         vFlags = map (\(n,t) -> (False, BName (NameVarMod n "s") (tSeries (TVar (UName kN)) t)))
                        vecs
     in  xApps (xVarOpSeries (OpSeriesRunProcess $ length vecs))
               (  map (XType .         snd) vecs
               ++ map (XVar  . UName . fst) vecs
               ++ [(makeXLamFlags (kFlags ++ vFlags) body)])

  -- Should we introduce a rate parameter for generates?
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
   = setreadSeriesX getMax tys n (mlookup "setread" tys n) x


setreadSeriesX 
        :: (Name -> Name) -> Map.Map Name TypeF -> Name -> TypeF -> ExpF -> ([LetsF], [LetsF])
setreadSeriesX getMax tys name ty xx
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
    , v                 <- getMax name -- canonName equivs name
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
    | [tA, p, vA]       <- args
    , XVar (UName nvA)  <- vA
    , tkA               <- klokT nvA
    , kA                <- klok nvA
    , TVar (UName nkT)  <- klokT name
    , tkT               <- klokT name
    -> XLet (LLet (BName name'flags (tBot kData))
                 $ xApps (xVarOpSeries (OpSeriesMap 1))
                         ([kA, tA, XType tBool, p, modNameX "s" vA]))
     $ xApps (xVarOpSeries (OpSeriesMkSel 1))
             ([kA, XVar (UName name'flags)
              ,    XLAM (BName nkT       kRate)
                 $ XLam (BName name'sel (tSel1 tkA tkT))
                 $ XLet (LLet (BName name's (tBot kData))
                             $ xApps (xVarOpSeries OpSeriesPack)
                                     ([kA, XType tkT, tA, XVar (UName name'sel), modNameX "s" vA]))
                         wrap'fill ])

   _
    -> xx
 | otherwise
 = xx

 where
  name'flags= NameVarMod name "flags"
  name'proc = NameVarMod name "proc"
  name'ref  = NameVarMod name "ref"
  name's    = NameVarMod name "s"
  name'sel  = NameVarMod name "sel"

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

modNameX :: String -> ExpF -> ExpF
modNameX s xx
 = case xx of
    XVar (UName n)
     -> XVar (UName (NameVarMod n s))
    _
     -> xx

-}


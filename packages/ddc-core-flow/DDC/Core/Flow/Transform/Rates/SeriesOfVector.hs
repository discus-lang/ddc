{-# OPTIONS_GHC -Wwarn #-}
module DDC.Core.Flow.Transform.Rates.SeriesOfVector
        (seriesOfVectorModule
        ,seriesOfVectorFunction)
where
import DDC.Core.Pretty
-- import DDC.Core.Collect
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Transform.Rates.Combinators   as Com
import DDC.Core.Flow.Transform.Rates.CnfFromExp
import DDC.Core.Flow.Transform.Rates.Fail
import DDC.Core.Flow.Transform.Rates.Graph
import qualified DDC.Core.Flow.Transform.Rates.SizeInference as SI
import DDC.Core.Flow.Transform.Rates.Linear
import DDC.Core.Module
import DDC.Core.Transform.Annotate
import DDC.Core.Transform.Deannotate

import qualified Data.Map as Map
import Data.Maybe (catMaybes)


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


   in  (mm { moduleBody = body' }, errs)
       


seriesOfVectorLets :: LetsF -> (LetsF, [(Name,Fail)])
seriesOfVectorLets ll
 | LLet b@(BName n _) x <- ll
 , (x',errs)  <- seriesOfVectorFunction x
 = (LLet b x', map (\f -> (n,f)) errs)

 | LRec bxs             <- ll
 , (bs,xs)              <- unzip bxs
 , (xs',_errs)          <- unzip $ map seriesOfVectorFunction xs
 = (LRec (bs `zip` xs'), []) 
        -- We still need to produce errors if this doesn't work.

 | otherwise
 = (ll, [])


-- | Takes a single function body. Function body must be in a-normal form.
seriesOfVectorFunction :: ExpF -> (ExpF, [Fail])
seriesOfVectorFunction fun
 = case cnfOfExp fun of
   Left err
    -> trace ("Error: " ++ show err)
             (fun, [FailCannotConvert err])
   Right prog
    -> trace ("2Converted: " ++ renderIndent (ppr prog)) $
          case SI.generate prog of
           Nothing
            -> trace ("3Error: can't perform size inference") (fun, [])
           Just (env,s)
            -> trace ("3SizeInf: " ++ renderIndent (ppr (env, s))) $
                 let g          = graphOfBinds prog env
                     tmap a b   = SI.parents prog env a b
                     clustering = solve_linear g tmap
                     clusters   = map snd $ Map.toList clustering
                     re         = reconstruct fun prog env clusters
                 in  trace ("4Graph: " ++ renderIndent (ppr (listOfGraph g)))
                   $ trace ("5Clust: " ++ renderIndent (ppr (Map.toList clustering)))
                   $ trace ("6OUT:   " ++ renderIndent (ppr $ annotate () re))
                   $ (re, [])

reconstruct
        :: ExpF
        -> Program Name Name
        -> SI.Env Name
        -> [[CName Name Name]]
        -> ExpF
reconstruct fun prog env clusters
 = let lets   = concatMap convert clusters
       (lams, body)   = takeXLamFlags_safe fun
       (_,    xx)     = splitXLets         body
   in  makeXLamFlags lams
     $ xLets lets
     $ snd $ splitXLets body -- xx
 where
  convert c
   = let outputs = outputsOfCluster prog c
         inputs  = inputsOfCluster prog c

         justArray (NameArray a) = [a]
         justArray _             = []

         arrIns  = concatMap justArray inputs

         binds   = catMaybes
                 $ map (lookupB prog) c
         binds'  = zipWith (\a a' -> (a, a' `elem` outputs)) binds c
     in  process arrIns binds'


process :: [Name]
        -> [(Com.Bind Name Name, Bool)]
        -> [LetsF]
process arrIns bs
 = let pres  = concatMap  getPre  bs
       mid   = runProcs  (mkProcs $ map fst bs)
       posts = concatMap  getPost bs
   in pres ++ [LLet (BNone tBool) mid] ++ posts
 where
  getPre b
   -- There is no point of having a reduce that isn't returned.
   | (SBind s (Fold _ (Seed z _) _), _) <- b
   = [LLet (bind $ NameVarMod s "ref") (xNew tYPE z)]

   -- For other bindings, if it isn't returned we needn't allocate anything
   | (_, False) <- b
   = []

   -- Returned vectors
   | (ABind v _, _) <- b
   = [LLet (bind v) (xNewVector tYPE allocSize)]

   | (Ext _ _ _, _) <- b
   = []


  getPost b
   | (SBind s (Fold _ (Seed z _) _), _) <- b
   = [ LLet (bind s) (xRead tYPE (var $ NameVarMod s "ref")) ]

   -- Ignore anything else
   | (_, _) <- b
   = []

  runProcs body
   = let kFlags = [ (True,  BName klok kRate)
                  , (False, BNone $ tRateNat $ TVar $ UName klok) ]
         vFlags = map (\n -> (False, BName (NameVarMod n "s") tYPE)) arrIns
     in  xApps (xVarOpSeries (OpSeriesRunProcess $ length arrIns))
               (  map (const $ xtYPE) arrIns
               ++ map (XVar  . UName)      arrIns
               ++ [(makeXLamFlags (kFlags ++ vFlags) body)])


  mkProcs (b:rs)
   | SBind s (Fold (Fun xf _) (Seed xs _) ain) <- b
   = let rest = mkProcs rs
     in  XLet (LLet   (bind $ NameVarMod s "proc")
              $ xApps (xVarOpSeries OpSeriesReduce)
                      [klokT, xtYPE, var (NameVarMod s "ref"), xf, xs, var (NameVarMod ain "s")])
         rest

   | ABind n abind <- b
   = let rest   = mkProcs rs
         n'proc = NameVarMod n "proc"
         n's    = NameVarMod n "s"
         n'flag = NameVarMod n "flags"
         n'k    = NameVarMod n "k"
         n'sel  = NameVarMod n "sel"

         go ll  = XLet ll rest

     in  case abind of
         MapN (Fun xf _) ains
          -> go $ LLet  (bind n's)
                $ xApps (xVarOpSeries (OpSeriesMap (length ains)))
                        ([klokT] ++ (replicate (length ains) $ xtYPE) ++ map var ains)

         Filter (Fun xf _) ain
          -> XLet (LLet (bind n'flag)
                      $  xApps (xVarOpSeries (OpSeriesMap 1))
                               [klokT, xtYPE, xtYPE, xf, var $ NameVarMod ain "s"])
           $ xApps (xVarOpSeries $ OpSeriesMkSel 1)
                   [ klokT, var n'flag
                   ,        XLAM (BName n'k kRate)
                          $ XLam (bind n'sel)
                          $ XLet (LLet (bind n's)
                                     $ xApps (xVarOpSeries OpSeriesPack)
                                       [xtYPE, xtYPE, xtYPE, var n'sel, var $ NameVarMod ain "s"])
                          $ rest]




  mkProcs []
   -- bs cannot be empty: there's no point of an empty cluster.
   = foldl1 mkJoin $ concatMap getProc bs

  mkJoin p q
   = xApps (xVarOpSeries OpSeriesJoin) [p, q]

  getProc (SBind s _, _)
   = [var $ NameVarMod s "proc"]
  getProc (ABind a _, True)
   = [xApps (xVarOpSeries OpSeriesFill) [xtYPE, xtYPE, var $ a, var $ NameVarMod a "s"]]
  getProc _
   = []

     
  allocSize
   -- If there are any inputs, use the size of one of those
   | (i:_) <- arrIns
   = xApps (xVarOpVector OpVectorLength) [xtYPE, var i]
   -- Or if it's a generate, find the size of the generate expression.
   | (sz:_) <- concatMap findGenerateSize bs
   = var sz
   -- XXX Otherwise it must be an external, and this won't be called...
   | otherwise
   = error ("ddc-core-flow: allocSize, but no size known" ++ show arrIns ++ "\n" ++ show bs)

  klok
   -- The name doesn't matter, we just need to choose one
   | ((ABind n _, _) :_) <- bs
   = NameVarMod n "k"
   | ((SBind n _, _) :_) <- bs
   = NameVarMod n "k"
   -- Otherwise it must be an external. It shouldn't reach here.
   | otherwise
   = error "ddc-core-flow: klok"

  klokT = XType $ TVar $ UName klok

  findGenerateSize (ABind _ (Generate sz _), _)
   = [sz]
  findGenerateSize _
   = []


  tYPE    = tBot kData
  xtYPE   = XType tYPE
  bind n  = BName n tYPE
  mod n s = NameVarMod
  var n   = XVar $ UName n


xVarOpSeries n = XVar (UPrim (NameOpSeries n) (typeOpSeries n))
xVarOpVector n = XVar (UPrim (NameOpVector n) (typeOpVector n))

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


{-# OPTIONS_GHC -Wwarn #-}
module DDC.Core.Flow.Transform.Rates.SeriesOfVector
        (seriesOfVectorModule
        ,seriesOfVectorFunction)
where
import DDC.Core.Collect
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp                           as DDC
import DDC.Core.Flow.Transform.Rates.Combinators   as Com
import DDC.Core.Flow.Transform.Rates.CnfFromExp
import DDC.Core.Flow.Transform.Rates.Fail
import DDC.Core.Flow.Transform.Rates.Graph
import qualified DDC.Core.Flow.Transform.Rates.SizeInference as SI
import DDC.Core.Flow.Transform.Rates.Clusters
import DDC.Core.Module
import DDC.Core.Transform.Annotate
import DDC.Core.Transform.Deannotate
import qualified DDC.Type.Env           as Env

import qualified Data.Map as Map
import           Data.Map   (Map)
import qualified Data.Set as Set
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
 = ( map (uncurry LLet) ls' ++ [LLet b x']
   , map (\f -> (n,f)) errs)

 | LRec bxs             <- ll
 , (bs,xs)              <- unzip bxs
 , (xs',ls', _errs)          <- unzip3 $ map seriesOfVectorFunction xs
 = ( [LRec (concat ls' ++ (bs `zip` xs'))]
   , []) 
        -- We still need to produce errors if this doesn't work.

 | otherwise
 = ([ll], [])


-- | Takes a single function body. Function body must be in a-normal form.
seriesOfVectorFunction :: ExpF -> (ExpF, [(BindF,ExpF)], [Fail])
seriesOfVectorFunction fun
 = case cnfOfExp fun of
   Left err
    -> (fun, [], [FailCannotConvert err])
   Right prog
    -> case SI.generate prog of
           Nothing
            -> (fun, [], [])
           Just (env,_s)
            -> let g          = graphOfBinds prog env
                   tmap a b   = SI.parents prog env a b
                   clusters   = cluster g tmap
                   (re, ls)   = (fun, []) -- reconstruct fun prog env clusters
               in  (re, ls, [])


{-
reconstruct
        :: ExpF
        -> Program Name Name
        -> SI.Env Name
        -> [[CName Name Name]]
        -> (ExpF, [(BindF, ExpF)])
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

         arrIns  = seriesInputsOfCluster prog c

         -- Map over the list of all binds and find only those that we want.
         -- This way is better than mapping lookup over c, as we get them in program order.
         binds   = filter (flip elem c . cnameOfBind) (_binds prog)
         binds'  = map (\a -> (a, cnameOfBind a `elem` outputs)) binds
         lets    = mkLets types env arrIns binds'
      in lets


-- | Extract processes out so they can be made into separate bindings
extractProcs :: [LetsF] -> [DDC.Bind Name] -> ([LetsF], [(BindF,ExpF)])
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
   , XVar (UPrim (NameOpSeries OpSeriesRunProcess ) _)    <- op
   , (xs, [lam])                                          <- splitAt (length args - 1) args

   = let fs = freeX Env.empty lam

         isMentioned bo
          | Just bo' <- takeSubstBoundOfBind bo
          = Set.member bo' fs
          | otherwise
          = False

         os = filter isMentioned e

         nm' = NameVarMod nm "process"

         x' = xApps op (xs ++ [xApps (XVar $ UName nm') (map XVar (takeSubstBoundsOfBinds os))])

         p' = makeXLamFlags (map (\o -> (False, o)) os) lam
     in ([LLet b x'], [(BName nm' (tBot kData),  p')])

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
--  1. filter arrIns to only those that must be series; second arg of gather, first of cross etc.
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
   | ((s, Left (Fold _ (Scalar z _) _)), _)       <- b
   = [LLet (BName (NameVarMod s "ref") $ tRef $ tyOf s) (xNew (tyOf s) z)]

   -- Returned vectors
   | ((v, Right _), True)                       <- b
   = [LLet (BName v $ tVector $ sctyOf v) (xNewVector (sctyOf v) allocSize)]

   -- Otherwise, it's not returned or we needn't allocate anything
   | _                                          <- b
   = []


  getPost b
   | ((s, Left (Fold _ (Scalar _ _) _)), _)       <- b
   = [ LLet (BName s $ sctyOf s) (xRead (tyOf s) (var $ NameVarMod s "ref")) ]

   -- Ignore anything else
   | _                                          <- b
   = []

  runProcs body
   = let kFlags = [ (True,  BName klok kRate)
                  , (False, BName (NameVarMod klok "r") $ tRateNat $ TVar $ UName klok) ]
         vFlags = map (\n -> (False, BName (NameVarMod n "s") (tSeries (klokT n) (sctyOf n)))) arrIns
     in  xApps (xVarOpSeries OpSeriesRunProcess)
               (  map xsctyOf arrIns
               ++ (if   null arrIns
                   then [allocSize]
                   else [])
               ++ map var    arrIns
               ++ [(makeXLamFlags (kFlags ++ vFlags) body)])


  mkProcs (b:rs)
   | ((s, Left (Fold (Fun xf _) (Scalar xs _) ain)), _)   <- b
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
                            
         Generate _sz (Fun xf _)
          -> llet n's (tSeries (klokT n) $ sctyOf n)
           ( xApps (xVarOpSeries OpSeriesGenerate)
                   [ klokX n, xsctyOf n, xf ])
             go

         Gather v ix
          -> llet n's (tSeries (klokT n) $ sctyOf n)
           ( xApps (xVarOpSeries OpSeriesGather)
                   ([klokX n, xsctyOf v, var v, var $ NameVarMod ix "s"]) )
             go




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
   | (Scalar sz _:_) <- concatMap findGenerateSize bs
   = sz
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


-}

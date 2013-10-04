module DDC.Core.Flow.Transform.Rates.SeriesOfVector
        (seriesOfVectorModule
        ,seriesOfVectorFunction)
where
import DDC.Core.Collect.Free
import DDC.Core.Collect.Free.Simple ()
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Transform.Rates.Fail
import DDC.Core.Flow.Transform.Rates.Constraints
import DDC.Core.Module
import DDC.Core.Transform.Annotate
import DDC.Core.Transform.Deannotate
import qualified DDC.Type.Env           as Env

import           Control.Applicative
import           Control.Monad
import           Data.List              (nub)
import qualified Data.Map               as Map
import           Data.Maybe             (catMaybes)
import qualified Data.Set               as Set

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

 | otherwise
 = (ll, [])


-- | Takes a single function body. Function body must be in a-normal form.
seriesOfVectorFunction :: ExpF -> (ExpF, [Fail])
seriesOfVectorFunction fun
 = run $ do
        -- Peel off the lambdas
        let (_lams, body)  = takeXLamFlags_safe fun
        -- TODO: Check that it's a-normal form
            (lets, _xx)    = splitXLets         body
        -- Split into name and values and warn for recursive bindings
        binds             <- takeLets           lets

        -- TODO check that binds are only vector primitives,
        -- OR   if not vector primitives, do not refer to bound vectors

        let names = map fst binds
        -- Make sure names are unique
        when (length names /= length (nub names)) $
          warn FailNamesNotUnique

        checkBindConstraints   binds

        _graph <- graphOfBinds binds

        return fun

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



-- | Graph for function
-- Each node is a binding, edges are dependencies, and the bool is whether the node's output can be fused or contracted.
-- For example, filter and map dependencies can be contracted,
-- but a fold cannot as it must consume the entire stream before producing output.
--

type Graph = Map.Map Name (Set.Set Name, Bool)

graphOfBinds :: [(Name,ExpF)] -> LogFailures Graph
graphOfBinds binds
 = return
 $ Map.fromList
 $ map gen
 $ binds
 where
  gen (k, xx)
   = let free = Set.fromList
              $ catMaybes
              $ map takeNameOfBound
              $ Set.toList
              $ freeX Env.empty xx
         refs = free `Set.intersection` names
     in  (k, (refs, contractible xx))

  names = Set.fromList
        $ map fst binds

  contractible xx
   | Just (f, _)                      <- takeXApps xx
   , XVar (UPrim (NameOpVector ov) _) <- f
   = case ov of
     OpVectorFoldIndex
      -> False
     -- TODO length of `concrete rate' is known before iteration, so should be contractible.
     OpVectorLength
      -> False
     _
      -> True

   | otherwise
   = True


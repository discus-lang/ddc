module DDC.Core.Flow.Transform.Rates.CnfFromExp
        (cnfOfExp) where
import DDC.Core.Collect
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Transform.Rates.Fail
import DDC.Core.Flow.Transform.Rates.Combinators        as CNF
import qualified DDC.Type.Env           as Env

import           Control.Monad
import           Data.List              (intersect, nub)
import           Data.Maybe             (catMaybes)
import           Data.Monoid
import qualified Data.Set               as Set


-----------------------------------
-- = Conversion from ExpF to CNF.
--
-- | Convert a given expression function to CNF.
-- For this to succeed, the function must:
--      - be in A-normal form, *except* worker functions should also be inlined
--      - all bindings are named, not de Bruijn indices
--      - names must be unique
--      - no recursive bindings
--      - no @letregion@s
--
-- If it succeeds, it should be true that
-- >>> expOfCnf . right . cnfOfExp = id
-- at least semantically, if not syntactically
-- 
cnfOfExp :: ExpF -> Either ConversionError (Program Name Name)
cnfOfExp fun
 = do   -- Peel off the lambdas
        let (lams, body)   = takeXLamFlags_safe fun
        -- Assuming the body is already in a-normal form.
            (lets, xx)     = splitXLets         body

        -- Split into name and values and warn for recursive bindings
        binds             <- takeLets           lets

        let lam_names = catMaybes $ map (takeNameOfBind . snd) lams
        let names     = lam_names ++ map fst binds
        -- Make sure names are unique
        when (length names /= length (nub names)) $
          Left FailNamesNotUnique

        -- For each value-level lambda binder, decide whether it's scalar or vector based on type
        let inputs  = mconcat $ map getInput lams
            getInput (False, BName n ty)
             -- Vectors on the right, scalars on the left
             | isTypeArray ty
             = ([],[n])
             | otherwise
             = ([n],[])
            getInput (_,_) = ([],[])

        -- For each binding, classify it as either array, scalar or external.
        --
        -- We must be careful about creating externals, though: if a binding is just a
        -- worker function, we don't really need that as an external.
        -- However, if we assume that all scalars will be fusion-preventing (they currently are),
        -- then creating externals for these will not affect scheduling.
        -- But what of worker functions referencing vectors? It becomes harder to outlaw if the
        -- worker function is not inlined into the combinator binding.
        -- Tuples are another potential problem here: looking at the tuple's type, it would not be
        -- an array binding.
        let (binds', env') = getBinds binds inputs 
        let outs           = localEnv env'  xx

        return (Program inputs binds' outs)

-- | Check if type is an array type, so we know whether variables are scalar or array.
-- This is perhaps a crude way to test, as what if the result of a fold is actually a vector?
-- Well, let's not worry about that right now.
isTypeArray :: TypeF -> Bool
isTypeArray = isVectorType


getBinds :: [(Name,(TypeF,ExpF))] -> ([Name],[Name]) -> ([CNF.Bind Name Name], ([Name],[Name]))
getBinds bs env
 = go bs env
 where
  go [] e = ([], e)
  go (b:rest) e
   = let b'          = getBind b e
         e'          = envOfBind b' <> e
         (rest',e'') = go rest e'
     in  (b' : rest', e'')


-- | Convert an epression to a CNF binding.
-- Assuming the incoming expression is well typed, this should never fail:
-- if we can't convert it to a "real" combinator, it will just be converted to an external.
--
-- Perhaps this is the wrong approach, and if a vector operator cannot be converted,
-- it should be an error or warning.
getBind :: (Name,(TypeF,ExpF)) -> ([Name], [Name]) -> CNF.Bind Name Name
getBind (nm,(t,x)) env
 -- Try to match against a known vector combinator.
 | Just (f, args) <- takeXApps x
 , XVar (UPrim (NameOpVector ov) _) <- f
 -- throw away that pesky type information
 , args' <- filter ((==Nothing) . takeXType) args
 = case (ov, args') of
   (OpVectorReduce, [worker, seed, arr])
    | Just fun     <- getFun worker
    , Just i       <- name seed
    , Just a       <- name arr
    -> SBind nm (Fold fun i a)

   (OpVectorMap n, worker : arrs)
    | Just fun       <- getFun worker
    , Just as        <- names  arrs
    , length arrs    == n
    -> ABind nm (MapN fun as)

   (OpVectorFilter, [worker, arr])
    | Just fun       <- getFun worker
    , Just a         <- name   arr
    -> ABind nm (Filter fun a)

   (OpVectorGenerate, [sz, worker])
    | Just fun       <- getFun worker
    , Just sz'       <- name   sz
    -> ABind nm (Generate sz' fun)

   _ | otherwise
    -> external

 -- It's not a vector combinator, so we'll have to create an external binding for it.
 | otherwise
 = external
 where
  external
   = let ins = localEnv env x
         out | isTypeArray t = ([], [nm])
             | otherwise     = ([nm], [])
     in  Ext out x ins

  names as
   | xs <- catMaybes $ map name as
   , length xs == length as
   = Just xs
   | otherwise
   = Nothing

  name xx
   | XVar (UName n) <- xx
   = Just n
    -- TODO: This isn't quite right..
   | XCon  dc       <- xx
   , Just  n        <- takeNameOfDaCon dc
   = Just n
   | otherwise
   = Nothing

  -- Try to extract a worker function from an expression.
  -- This fails if the worker function mentions any local arrays.
  -- I'm not sure if failing in this case is strictly necessary; it should just be nonfusible edges.
  getFun xx
   = let (ss, as) = localEnv env xx
         -- Check that no local arrays are referenced
     in  if   null as
         then Just $ Fun xx ss
         else Nothing


-- | Find local variables that are mentioned in expression, sorted into scalar and array
localEnv :: ([Name],[Name]) -> ExpF -> ([Name],[Name])
localEnv env xx
       -- Get all the free variables mentioned in exp
 = let free = catMaybes
            $ map takeNameOfBound
            $ Set.toList
            $ freeX Env.empty xx
       -- Limit to just the scalar references
       ss = free `intersect` fst env
       -- and array refs
       as = free `intersect` snd env
       -- Check that no local arrays are referenced
  in  (ss, as)


-- | Peel the lambdas off, or leave it alone if there are none
takeXLamFlags_safe x
 | Just (binds, body) <- takeXLamFlags x
 = (binds, body)
 | otherwise
 = ([],    x)


-- | Split into name and values and error for outlawed bindings
takeLets :: [LetsF] -> Either ConversionError [(Name, (TypeF, ExpF))]
takeLets lets
 = mapM get lets
 where
  get (LLet (BName n t) x) = return (n,(t,x))
  get (LLet (BNone _)   _) = Left   FailNoAnonAllowed
  get (LLet (BAnon _)   _) = Left   FailNoDeBruijnAllowed
  get (LRec        _     ) = Left   FailRecursiveBindings
  get (LPrivate _ _ _)     = Left   FailLetRegionNotHandled
  get (LWithRegion _     ) = Left   FailLetRegionNotHandled



module DDC.Core.Transform.Unshare
        ( unshareModule)
where
import DDC.Core.Exp.Annot.AnTEC
import DDC.Core.Exp.Annot
import DDC.Core.Module
import DDC.Type.Transform.SubstituteT
import Data.Map                         (Map)
import qualified Data.Map.Strict        as Map


-------------------------------------------------------------------------------
-- | Apply the unsharing transform to a module.
unshareModule 
        :: (Ord n, Show n)
        => Module (AnTEC a n) n -> Module (AnTEC a n) n

unshareModule !mm
 = let
        -- Add extra parameters to the CAFs,
        -- returning the names of the ones we've transformed
        -- along with the transformed module body.
        (nts, xx)   = addParamsX $ moduleBody mm

        -- Add the corresponding arguments to each use.
        xx'         = addArgsX nts xx
 
   in   mm { moduleBody = xx' }
 

 -------------------------------------------------------------------------------
-- | Add unit parameters to the top-level CAFs in the given module body,
--   returning a map of names of transformed CAFs to their transformed 
--   types.
addParamsX 
        :: Ord n
        => Exp (AnTEC a n) n    --  Module body to transform.
        -> ( Map n (Type n)     --  Map of transformed bindings to their
                                --    transformed types.
           , Exp (AnTEC a n) n) --  Transformed module body.

addParamsX xx
 = case xx of
        -- Transform all the top-level bindings of a module body.
        XLet a (LRec bxs) xBody
          -> let (ns, bxs') = addParamsBXS a bxs
             in  ( ns
                 , XLet a (LRec bxs') xBody)

        _ ->     ( Map.empty
                 , xx)


-- | Add unit parameters to the bound CAFs in the given list.
addParamsBXS _a []
 =      (Map.empty, [])

addParamsBXS a ((b, x) : bxs)
 = let  (ns1, b', x') = addParamsBX  a b x
        (ns2, bxs')   = addParamsBXS a bxs
   in   ( Map.union ns1 ns2
        , (b', x') : bxs')


-- | Add unit parameter to a single top-level binding, if it needs one.
addParamsBX _ b@(BName n _) x
 = case addParamsBodyX x of
        Nothing 
         -> (Map.empty, b, x)

        Just (x', t')
         -> ( Map.singleton n t'
            , replaceTypeOfBind t' b
            , x')

addParamsBX _ b x
  =     (Map.empty, b, x)


-- | Add unit parameters to the right of a let-binding.
addParamsBodyX xx
 = case xx of
        -- This binding already has an outer value abstraction,
        -- so we don't need to add any more.
        XLam{}  
         -> Nothing

        -- Decend under type abstractions. To keep the supers
        -- in standard form with all the type abstractions first, 
        -- if we need to add a value abstraction we want to add it
        -- under exising type abstractions.
        XLAM a bParam xBody
         -> case addParamsBodyX xBody of
                Nothing 
                 -> Nothing

                Just (xBody', tBody')
                 -> let t' = TForall bParam tBody'
                        a' = a { annotType = t' }
                    in  Just ( XLAM a' bParam xBody', t')

        -- We've hit a plain value, so need to wrap it in a 
        -- value abstraction.
        _   
         -> let a  = annotOfExp xx
                t' = tFun tUnit (annotType a)
                a' = a { annotType = t' }
            in  Just (XLam a' (BNone tUnit) xx, t')


-------------------------------------------------------------------------------
-- | Decend into an expression looking for applications of CAFs that 
--   we've already added an extra unit parameter to. When we find them,
--   add the matching unit argument.
--
addArgsX :: (Show n, Ord n)
         =>  Map n (Type n)     -- ^ Map of names of CAFs that we've added 
                                --   parameters to, to their transformed types.
         ->  Exp (AnTEC a n) n  -- ^ Transform this expression.
         ->  Exp (AnTEC a n) n  -- ^ Transformed expression.

addArgsX nts xx
 = let  downX   = addArgsX   nts
        downLts = addArgsLts nts
        downA   = addArgsAlt nts

   in  case xx of

        -- Add an extra argument for a monomorphic CAF.
        XVar _a (UName n)
         -> case Map.lookup n nts of
                Just tF -> fst $ wrapAppX xx tF
                Nothing -> xx

        XVar{}            -> xx
        XCon{}            -> xx

        XApp{}            -> addArgsAppX nts xx []

        -- For the rest of the constructs their types do not
        -- change during the transform so we can resuse the old ones.
        XLAM a b xBody    -> XLAM  a b (downX xBody)
        XLam a b xBody    -> XLam  a b (downX xBody)
        XLet a lts xBody  -> XLet  a   (downLts lts)  (downX xBody)
        XCase a xScrut as -> XCase a   (downX xScrut) (map downA as)
        XCast a c x       -> XCast a c (downX x)
        XType{}           -> xx
        XWitness{}        -> xx


addArgsAppX !nts !xx !ats
 = let  downX   = addArgsX nts
        tA      = annotType $ annotOfExp xx
   in  case xx of
        XVar _a (UName n)
         -> case Map.lookup n nts of
                Just tF 
                 -> let (x1, t1) = wrapAtsX xx tF ats
                        (x2, _)  = wrapAppX x1 t1 
                    in  x2

                Nothing 
                 -> fst $ wrapAtsX xx tA ats

        XVar{} 
          -> fst $ wrapAtsX xx tA ats

        XCon{} 
          -> fst $ wrapAtsX xx tA ats

        XApp _a1 x1 (XType a2 t)
          -> addArgsAppX nts x1 ((a2, t) : ats)

        XApp a x1 x2
          -> XApp a (addArgsAppX nts x1 ats) (downX x2)

        _ -> fst $ wrapAtsX xx tA ats


addArgsLts nts lts
 = let downX = addArgsX nts
   in  case lts of
        LLet b x        -> LLet b (downX x)
        LRec bxs        -> LRec [(b, downX x) | (b, x) <- bxs]
        LPrivate{}      -> lts


addArgsAlt nts aa 
 = let downX = addArgsX nts
   in case aa of
        AAlt p x        -> AAlt p (downX x)


-- Wrap an expression with an application of a unit value.
wrapAppX :: Exp (AnTEC a n) n 
         -> Type n
         -> (Exp (AnTEC a n) n,   Type n)

wrapAppX xF tF
 = case takeTFun tF of
    Just (_, tResult)
     -> let a   = annotOfExp xF
            aR  = a { annotType = tResult }
            aV  = a { annotType = tF      }
            aU  = a { annotType = tUnit   }
            xF' = mapAnnotOfExp (const aV) xF
        in  ( XApp aR xF' (xUnit aU)
            , tResult)

    Nothing 
     -> (xF, tF)


-- Apply the given type arguments to an expression.
wrapAtsX !xF !tF []
 = (xF, tF)

wrapAtsX !xF !tF ((aArg, tArg): ats)
 = case tF of 
    TForall bParam tBody
     -> let a   = annotOfExp xF
            tR  = substituteT bParam tArg tBody
            aR  = a { annotType = tR }
            aV  = a { annotType = tF }
            xF' = mapAnnotOfExp (const aV) xF
        in  wrapAtsX
                (XApp aR xF' (XType aArg tArg))
                tR ats

    _ -> (xF, tF)


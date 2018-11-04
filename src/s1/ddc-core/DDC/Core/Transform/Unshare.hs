
module DDC.Core.Transform.Unshare
        (unshareModule)
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
        :: (Show a, Ord n, Show n)
        => Module (AnTEC a n) n -> Module (AnTEC a n) n

unshareModule !mm
 = let
        -- Add extra parameters to the types of imported CAFs.
        importValuesNts
                = [ let (iv', m) = addParamsImportValue iv
                    in  ((n, iv'), m)
                  | (n, iv) <- moduleImportValues mm]

        (importValues', ntssImport')
                = unzip importValuesNts

        -- Add extra parameters to the CAFs,
        -- returning the names of the ones we've transformed
        -- along with the transformed module body.
        (ntsBody, xx) = addParamsX $ moduleBody mm

        -- Add the corresponding arguments to each use.
        nts'    = Map.union (Map.unions ntssImport') ntsBody
        xx'     = addArgsX nts' xx

        -- Update the types of exports with the transformed ones.
        exportValues'
                = [ (n, updateExportValue nts' ex)
                  | (n, ex) <- moduleExportValues mm ]

   in   mm { moduleBody         = xx'
           , moduleExportValues = exportValues'
           , moduleImportValues = importValues' }


-------------------------------------------------------------------------------
-- | If this import def imports a CAF then then add an extra parameter to its
--   type, assuming that the unsharing transform has also been applied to the
--   imported module.
--
addParamsImportValue
        ::  ImportValue n (Type n)
        -> (ImportValue n (Type n), Map n (Type n))

addParamsImportValue iv
 = case iv of
        ImportValueModule m n t (Just (nType, nValue, nBoxes))
         -> case addParamsT t of
                Just t'
                 -> ( ImportValueModule m n t'
                        (Just (nType, nValue + 1, nBoxes))
                    , Map.singleton n t')

                Nothing
                 -> ( iv, Map.empty)

        ImportValueModule{} -> (iv, Map.empty)
        ImportValueSea{}    -> (iv, Map.empty)


-- | If this is the type of a CAF then add an extra unit parameter to it.
addParamsT :: Type n -> Maybe (Type n)
addParamsT tt
 = case tt of
        TVar{}  -> Just $ tUnit `tFun` tt
        TCon{}  -> Just $ tUnit `tFun` tt

        TAbs b tBody
         -> do  tBody'   <- addParamsT tBody
                return   $  TAbs b tBody'

        TApp{}
         -> case takeTFun tt of
                Nothing -> Just $ tUnit `tFun` tt
                Just _  -> Nothing

        TForall b tBody
         -> do  tBody'   <- addParamsT tBody
                return   $  TForall b tBody'

        TSum{} -> Nothing
        TRow{} -> Nothing


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
        XAbs _ MTerm{}     _ -> Nothing
        XAbs _ MImplicit{} _ -> Nothing

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
addArgsX :: (Show n, Ord n, Show a)
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
        XVar a (UName n)
         -> case Map.lookup n nts of
                Just tF
                 -> let xx'     = XVar (a { annotType = tF }) (UName n)
                    in  wrapAppX a tF xx'

                Nothing   -> xx

        XVar{}            -> xx
        XAtom{}           -> xx
        XApp{}            -> addArgsAppX nts xx []

        -- For the rest of the constructs their types do not
        -- change during the transform so we can resuse the old ones.
        XAbs a b xBody    -> XAbs   a b (downX xBody)
        XLet a lts xBody  -> XLet   a   (downLts lts)  (downX xBody)
        XCase a xScrut as -> XCase  a   (downX xScrut) (map downA as)
        XCast a c x       -> XCast  a c (downX x)
        XAsync a b e1 e2  -> XAsync a b (downX e1)     (downX e2)


addArgsAppX !nts !xx !ats
 = let  downX   = addArgsX nts
        tA      = annotType $ annotOfExp xx
   in  case xx of
        XVar a (UName n)
         -> case Map.lookup n nts of
                Just tF
                 -> let xx'      = XVar (a { annotType = tF }) (UName n)
                        (x1, t1) = wrapAtsX xx' tF ats
                        x2       = wrapAppX a   t1 x1
                    in  x2

                Nothing
                 -> fst $ wrapAtsX xx tA ats

        XVar{}
          -> fst $ wrapAtsX xx tA ats

        XCon{}
          -> fst $ wrapAtsX xx tA ats

        XApp a1 x1 (RType t)
          -> addArgsAppX nts x1 ((a1, t) : ats)

        XApp  a x1 (RTerm x2)
          -> XApp a (addArgsAppX nts x1 ats)
                    (RTerm (downX x2))

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
wrapAppX :: (Show a, Show n)
         => AnTEC a n
         -> Type n
         -> Exp (AnTEC a n) n
         -> Exp (AnTEC a n) n

wrapAppX a tF xF
 | Just (_, tResult)    <- takeTFun tF
 = let  a'  = annotOfExp xF
        aR  = a' { annotType = tResult }
        aV  = a' { annotType = tF      }
        aU  = a' { annotType = tUnit   }
        xF' = mapAnnotOfExp (const aV) xF
   in   XApp aR xF' (RTerm (xUnit aU))


 -- ISSUE #384: Unshare transform produces AST node with wrong type annotation.
 | Just (bs, tBody)     <- takeTForalls tF
 = let  Just us = sequence
                $ map takeSubstBoundOfBind bs

        xF'     = makeXLamFlags a [(True, b) | b <- bs]
                $ wrapAppX a tBody
                $ xApps a xF (map RType $ map TVar us)

   in   xF'


 | otherwise
 = xF


-- Apply the given type arguments to an expression.
wrapAtsX !xF !tF []
 = (xF, tF)

wrapAtsX !xF !tF ((_aArg, tArg): ats)
 = case tF of
    TForall bParam tBody
     -> let a   = annotOfExp xF
            tR  = substituteT bParam tArg tBody
            aR  = a { annotType = tR }
            aV  = a { annotType = tF }
            xF' = mapAnnotOfExp (const aV) xF
        in  wrapAtsX
                (XApp aR xF' (RType tArg))
                tR ats

    _ -> (xF, tF)


-------------------------------------------------------------------------------
-- | Update the types of exported things with the ones in
--   the give map.
updateExportValue
        :: Ord n
        => Map n (Type n)
        -> ExportValue n (Type n) -> ExportValue n (Type n)

updateExportValue mm ex
 = case ex of
        ExportValueLocal mn n _t mArity
         -> case Map.lookup n mm of
                Nothing -> ex
                Just t' -> ExportValueLocal mn n t' mArity

        ExportValueLocalNoType{} -> ex
        ExportValueSea{}         -> ex



module DDC.Core.Discus.Convert.Exp.PrimCall
        (convertPrimCall)
where
import DDC.Core.Discus.Transform.Curry.Callable
import DDC.Core.Discus.Convert.Exp.Arg
import DDC.Core.Discus.Convert.Exp.Base
import DDC.Core.Discus.Convert.Type
import DDC.Core.Discus.Convert.Error
import DDC.Type.Transform.Instantiate
import DDC.Core.Exp.Annot
import DDC.Data.Pretty
import DDC.Core.Check                    (AnTEC(..))
import qualified Data.Map                as Map
import qualified DDC.Core.Call           as Call
import qualified DDC.Core.Discus.Prim    as E
import qualified DDC.Core.Module         as C
import qualified DDC.Core.Salt.Runtime   as A
import qualified DDC.Core.Salt.Name      as A
import qualified DDC.Core.Salt.Compounds as A
import qualified Data.Text               as T
import Data.Maybe


-- Env --------------------------------------------------------------------------------------------
-- TOOD: reuse this at the construction site.
nameOfInfoIndexSuperRef :: C.ModuleName -> T.Text -> T.Text
nameOfInfoIndexSuperRef (C.ModuleName parts) txCtorName
 = let  mn' = T.intercalate (T.pack ".") $ map T.pack parts
   in   "ddcInfoIndex.super." % mn' % "." % txCtorName


-- Convert ----------------------------------------------------------------------------------------
-- | Convert a Discus function call primitive to Salt.
convertPrimCall
        :: ExpContext                   -- ^ The surrounding expression context.
        -> Context a                    -- ^ Types and values in the environment.
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> Maybe (ConvertM a (Exp a A.Name))

convertPrimCall _ectx ctx xx
 = let  convertX  = contextConvertExp  ctx
        downArgX  = convertX           ExpArg ctx
   in case xx of
        ---------------------------------------------------
        -- Reify a top-level super.
        XApp (AnTEC _t _ _ a)  xa xb
         | (xR,   [RType _, RType _, RTerm xF]) <- takeXApps1 xa xb
         , XVar _ (UName nR)        <- xR
         , E.NameOpFun E.OpFunReify <- nR

           -- Given the expression defining the super, retrieve its
           -- value arity and any extra type arguments we need to apply.
         , Just (xF_super, nSuper, tSuper, csCall, tksArgs)
            <- case xF of
                XVar aF (UName nF)
                 -- This variable was let-bound to the application of a super
                 -- name to some type arguments, like f = g [t1] [t2].
                 -- The value arity and extra type arguments we need to add are
                 -- are stashed in the ConvertM state monad.
                 -- See [Note: Binding top-level supers]
                 --
                 -- ISSUE #350: Discus to Salt conversion of let-bound type
                 --    applications is incomplete.
                 --
                 --    The following process won't work with code like:
                 --       like f  = g1 [t1] [t2]
                 --            g1 = g2 [t3] [t4] [t5]
                 --    as we don't look through the intermediate g1 binding
                 --    to see the other type args. These should really be
                 --    inlined in a pre-process.
                 --
                 |  Just (nSuper, tksArgs)
                        <- Map.lookup nF (contextSuperBinds ctx)
                 -> let
                        uSuper          = UName nSuper
                        xF'             = XVar aF uSuper

                        -- Lookup the call pattern of the super.
                        --  If this fails then the super name is in-scope, but
                        --  we can't see its definition in this module, or
                        --  salt-level import to get the arity.
                        Just callable   = Map.lookup nSuper (contextCallable ctx)
                        tSuper          = typeOfCallable callable
                        csSuper         = consOfCallable callable

                    in  Just (xF', nSuper, tSuper, csSuper, tksArgs)

                 -- The name is that of an existing top-level super, either
                 -- defined in this module or imported from somewhere else.
                 | otherwise
                 -> let
                        -- Lookup the call pattern of the super.
                        --   If this fails then the super name is in-scope, but
                        --   we can't see its definition in this module, or
                        --   salt-level import to get the arity.
                        Just callable   = Map.lookup nF    (contextCallable ctx)
                        tSuper          = typeOfCallable callable
                        csSuper         = consOfCallable callable

                    in  Just (xF, nF, tSuper, csSuper, [])

                _ -> Nothing

         -> Just $ do

                -- Apply any outer type arguments to the functional expression.
                xF_super'   <- downArgX xF_super

                xsArgs'     <- fmap catMaybes
                            $  mapM (convertOrDiscardSuperArgX ctx)
                                [ (RType tArg, kArg) | (tArg, kArg) <- tksArgs]

                let xF'     = xApps a xF_super' xsArgs'

                -- Type of the super with its type args applied.
                let Just tSuper' = instantiateTs tSuper $ map fst tksArgs

                -- Discharge type abstractions with type args that are applied
                -- directly to the super.
                let (csCall', [])
                        = Call.dischargeConsWithElims csCall
                        $ [Call.ElimType a t | t <- map fst tksArgs]

                let Just (_csType, csValue, csBoxes)
                        = Call.splitStdCallCons csCall

                let squashName (E.NameVar tx)   = tx
                    squashName (E.NameExt n tx) = squashName n <> "$" <> tx
                    squashName _
                     = error $ "ddc-core-discus.convertPrimCall: invalid super name "
                             ++ show nSuper

                -- Get the info table index for this super.
                -- TODO: we don't have the real module name available.
                let xInfoIndex nSuper'
                     = case nSuper' of
                        E.NameVar txSuperName
                         -> let txInfoRef
                                 = nameOfInfoIndexSuperRef
                                        (C.ModuleName ["DDC"]) txSuperName
                            in  A.xRead a (A.tWord 32)
                                          (A.xGlobal a (A.tWord 32) txInfoRef)
                                          (A.xNat a 0)

                        E.NameExt{}
                          -> xInfoIndex (E.NameVar $ squashName nSuper')

                        _ -> error $ "ddc-core-discus.convertPrimCall: invalid super name "
                                   ++ show nSuper

                -- Get the Sea-level type of the super.
                --   We need to use the call pattern here to detect the case
                --   where the super returns a functional value. We can't do
                --   this directly from the Discus-level type.
                tF' <- convertSuperConsT (typeContext ctx) csCall' tSuper'

                return
                 $ A.xAllocThunk a A.rTop
                        (xConvert a A.tAddr tF' xF')
                        (xInfoIndex nSuper)
                        (A.xNat a $ fromIntegral $ length csValue)
                        (A.xNat a $ fromIntegral $ length csBoxes)
                        (A.xNat a 0)                                -- args
                        (A.xNat a 0)                                -- runs


        ---------------------------------------------------
        -- Curry arguments onto a reified function.
        --   This works for both the 'curryN#' and 'extendN#' primops,
        --   as they differ only in the Discus-level closure type.
        XApp (AnTEC _t _ _ a) xa xb
         | (x1, args)            <- takeXApps1 xa xb
         , XVar _ (UName nPrim)  <- x1

         , Just nArgs
            <- case nPrim of
                E.NameOpFun (E.OpFunCurry   nArgs) -> Just nArgs
                _                                  -> Nothing

         , Just tsArg            <- sequence $ map takeRType $ take nArgs args
         , Just (xThunk : xsArg) <- sequence $ map takeRTerm $ drop (nArgs + 1) args
         , nArgs == length xsArg
         -> Just $ do
                xThunk'         <- downArgX xThunk
                xsArg'          <- mapM downArgX xsArg
                tsArg'          <- mapM (convertDataT (typeContext ctx)) tsArg
                let bObject     = BAnon (A.tPtr A.rTop A.tObj)
                let bArgs       = BAnon A.tNat

                return
                 $ XLet  a (LLet bObject
                                 (A.xExtendThunk     a A.rTop A.rTop xThunk'
                                        (A.xNat a $ fromIntegral nArgs)))
                 $ XLet  a (LLet bArgs
                                 (A.xArgsOfThunk    a A.rTop xThunk'))

                 $ xLets a [LLet (BNone A.tVoid)
                                 (A.xSetFieldOfThunk a
                                        A.rTop           -- region containing thunk.
                                        tPrime           -- region containing new child.
                                        (XVar a (UIx 1)) -- new thunk.
                                        (XVar a (UIx 0)) -- base index
                                        (A.xNat a ix)    -- offset
                                        (xArg))
                                 | ix   <- [0..]
                                 | xArg <- xsArg'
                                 | tArg <- tsArg'
                                 , let tPrime   = fromMaybe A.rTop
                                                $ takePrimeRegion tArg ]

                 $ XVar a (UIx 1)


        ---------------------------------------------------
        -- Apply a thunk.
        XApp (AnTEC _t _ _ a) xa xb
         | (x1, args)            <- takeXApps1 xa xb
         , XVar _ (UName nPrim)  <- x1
         , Just nArgs
            <- case nPrim of
                E.NameOpFun (E.OpFunApply  nArgs) -> Just nArgs
                _                                 -> Nothing

         , Just tsArg           <- sequence $ map takeRType $ take nArgs args
         , Just (xF : xsArgs)   <- sequence $ map takeRTerm $ drop (nArgs + 1) args
         -> Just $ do
                -- Functional expression.
                xF'     <- downArgX xF

                -- Arguments and their ypes.
                xsArg'  <- mapM downArgX xsArgs
                tsArg'  <- mapM (convertDataT (typeContext ctx)) tsArg

                -- Evaluate a thunk, returning the resulting Addr#,
                -- then cast it back to a pointer of the appropriate type
                return
                 $ A.xApplyThunk a nArgs
                        A.rTop
                        [  fromMaybe A.rTop $ takePrimeRegion tArg'
                                | tArg' <- tsArg']
                        A.rTop xF' xsArg'


        ---------------------------------------------------
        -- This isn't a call primitive.
        _ -> Nothing


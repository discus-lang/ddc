
module DDC.Core.Tetra.Convert.Exp.PrimCall
        (convertPrimCall)
where
import DDC.Core.Tetra.Convert.Exp.Arg
import DDC.Core.Tetra.Convert.Exp.Base
import DDC.Core.Tetra.Convert.Callable
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Error
import DDC.Type.Transform.Instantiate
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Check                    (AnTEC(..))
import qualified Data.Map                as Map
import qualified DDC.Core.Call           as Call
import qualified DDC.Core.Tetra.Prim     as E
import qualified DDC.Core.Salt.Runtime   as A
import qualified DDC.Core.Salt.Name      as A
import qualified DDC.Core.Salt.Compounds as A
import Data.Maybe


-- | Convert a Tetra function call primitive to Salt.
convertPrimCall
        :: Show a 
        => ExpContext           -- ^ The surrounding expression context.
        -> Context a            -- ^ Types and values in the environment.
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> Maybe (ConvertM a (Exp a A.Name))

convertPrimCall _ectx ctx xx
 = let  convertX  = contextConvertExp  ctx
        downArgX  = convertX           ExpArg ctx 

   in case xx of

        ---------------------------------------------------
        -- Reify a top-level super.
        --  TODO: Check that we're only reifying functions that will have
        --        the standard calling convention.
        XApp (AnTEC _t _ _ a)  xa xb
         | (xR,   [XType _ _, XType _ _, xF])   <- takeXApps1 xa xb
         , XVar _ (UPrim nR _tPrim)     <- xR
         , E.NameOpFun E.OpFunCReify    <- nR

           -- Given the expression defining the super, retrieve its
           -- value arity and any extra type arguments we need to apply.
         , Just (xF_super, tSuper, csCall, atsArg)
            <- case xF of
                XVar aF (UName nF)
                 -- This variable was let-bound to the application of a super
                 -- name to some type arguments, like f = g [t1] [t2]. 
                 -- The value arity and extra type arguments we need to add are
                 -- are stashed in the ConvertM state monad.
                 -- See [Note: Binding top-level supers]
                 --
                 -- TODO: check this works with repeated bindings,
                 --       like f  = g1 [t1] [t2]
                 --            g1 = g2 [t3] [t4] [t5]
                 --
                 |  Just (nSuper, atsArgs) 
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

                    in  Just (xF', tSuper, csSuper, atsArgs)

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

                    in  Just (xF, tSuper, csSuper, [])

                _ -> Nothing

         -> Just $ do

                -- Apply any outer type arguments to the functional expression.
                xF_super'   <- downArgX xF_super

                xsArgs'     <- fmap catMaybes
                            $  mapM (convertOrDiscardSuperArgX xx ctx) 
                            $  [XType aArg tArg | (aArg, tArg) <- atsArg]

                let xF'     = xApps a xF_super' xsArgs'

                -- Type of the super with its type args applied.
                let Just tSuper' = instantiateTs tSuper $ map snd atsArg

                -- Discharge type abstractions with type args that are applied
                -- directly to the super.
                let (csCall', []) 
                        = Call.dischargeConsWithElims csCall 
                        $ [Call.ElimType a a t | t <- map snd atsArg]

                let Just (_csType, csValue, csBoxes)
                        = Call.splitStdCallCons csCall

                -- Get the Sea-level type of the super.
                --   We need to use the call pattern here to detect the case
                --   where the super returns a functional value. We can't do
                --   this directly from the Tetra-level type.
                tF'       <- convertSuperConsT (typeContext ctx) csCall' tSuper'

                return  $ A.xAllocThunk a A.rTop 
                                (xConvert a A.tAddr tF' xF')
                                (A.xNat a $ fromIntegral $ length csValue)
                                (A.xNat a $ fromIntegral $ length csBoxes)
                                (A.xNat a 0)                                -- args
                                (A.xNat a 0)                                -- runs


        ---------------------------------------------------
        -- Curry arguments onto a reified function.
        --   This works for both the 'curryN#' and 'extendN#' primops,
        --   as they differ only in the Tetra-level closure type.
        XApp (AnTEC _t _ _ a) xa xb
         | (x1, xs)                     <- takeXApps1 xa xb
         , XVar _ (UPrim nPrim _tPrim)  <- x1

         , Just nArgs   
            <- case nPrim of 
                E.NameOpFun (E.OpFunCurry   nArgs) -> Just nArgs
                E.NameOpFun (E.OpFunCCurry  nArgs) -> Just nArgs
                E.NameOpFun (E.OpFunCExtend nArgs) -> Just nArgs
                _                                  -> Nothing

         , tsArg              <- [tArg | XType _ tArg <- take nArgs xs]
         , (xThunk : xsArg)   <- drop (nArgs + 1) xs
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
                                        A.rTop                  -- region containing thunk.
                                        tPrime                  -- region containing new child.
                                        (XVar a (UIx 1))        -- new thunk.
                                        (XVar a (UIx 0))        -- base index
                                        (A.xNat a ix)           -- offset
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
         | (x1, xs)                           <- takeXApps1 xa xb
         , XVar _ (UPrim nPrim _tPrim)        <- x1
         , Just nArgs
            <- case nPrim of
                E.NameOpFun (E.OpFunApply  nArgs) -> Just nArgs
                E.NameOpFun (E.OpFunCApply nArgs) -> Just nArgs
                _                                 -> Nothing

         , tsArg                <- [tArg | XType _ tArg <- take nArgs xs]
         , xF : xsArgs          <- drop (nArgs + 1) xs
         -> Just $ do
                -- Functional expression.
                xF'             <- downArgX xF

                -- Arguments and their ypes.
                xsArg'          <- mapM downArgX xsArgs
                tsArg'          <- mapM (convertDataT (typeContext ctx)) tsArg

                -- Evaluate a thunk, returning the resulting Addr#, 
                -- then cast it back to a pointer of the appropriate type
                return  $ A.xApplyThunk a nArgs 
                        $   [ XType a A.rTop ]

                         ++ [ XType a $ fromMaybe A.rTop $ takePrimeRegion tArg'
                                | tArg'         <- tsArg']

                         ++ [ XType a A.rTop ]
                         ++ [ xF' ]
                         ++ xsArg'


        ---------------------------------------------------
        -- This isn't a call primitive.
        _ -> Nothing



module DDC.Core.Tetra.Convert.Exp.PrimCall
        (convertPrimCall)
where
import DDC.Core.Tetra.Convert.Exp.Base
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Check                    (AnTEC(..))
import qualified DDC.Core.Tetra.Prim     as E
import qualified DDC.Core.Salt.Runtime   as A
import qualified DDC.Core.Salt.Name      as A
import qualified DDC.Core.Salt.Compounds as A
import Data.Maybe


-- | Convert a Tetra function call primitive to Salt.
convertPrimCall
        :: Show a 
        => ExpContext                   -- ^ The surrounding expression context.
        -> Context                      -- ^ Types and values in the environment.
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> Maybe (ConvertM a (Exp a A.Name))

convertPrimCall _ectx ctx xx
 = let  defs      = contextDataDefs    ctx
        kenv      = contextKindEnv     ctx
        convertX  = contextConvertExp  ctx
        downArgX  = convertX           ExpArg ctx 

   in case xx of

        ---------------------------------------------------
        -- Reify a top-level super.
        --  TODO: Check that we're only reifying functions that will have
        --        the standard calling convention.
        XApp (AnTEC _t _ _ a)  xa xb
         | (x1, [XType _ t1, XType _ t2, xF]) <- takeXApps1 xa xb
         , XVar _ (UPrim nPrim _tPrim)  <- x1
         , E.NameOpFun E.OpFunCReify    <- nPrim
         , XVar _ uF                    <- xF
         -> Just $ do
                xF'     <- downArgX xF
                tF'     <- convertRepableT defs kenv (tFun t1 t2)
                let Just arity = superDataArity ctx uF

                return  $ A.xAllocThunk a A.rTop 
                                (xConvert a A.tAddr tF' xF')
                                (A.xNat a $ fromIntegral arity)
                                (A.xNat a 0)


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
                tsArg'          <- mapM (convertValueT defs kenv) tsArg
                let bObject     = BAnon (A.tPtr A.rTop A.tObj)
                let bAvail      = BAnon A.tNat

                return 
                 $ XLet  a (LLet bObject 
                                 (A.xExtendThunk     a A.rTop A.rTop xThunk' 
                                        (A.xNat a $ fromIntegral nArgs)))
                 $ XLet  a (LLet bAvail
                                 (A.xAvailOfThunk    a A.rTop xThunk'))

                 $ xLets a [LLet (BNone A.tVoid)
                                 (A.xSetFieldOfThunk a A.rTop 
                                        (XVar a (UIx 1))                 -- new thunk
                                        (XVar a (UIx 0))                 -- base index
                                        (A.xNat a ix)                    -- offset
                                        (xTakePtr a tPrime A.tObj xArg)) -- value
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
         , XType _ tResult : _  <- drop  nArgs xs
         , xF : xsArgs          <- drop (nArgs + 1) xs
         -> Just $ do
                -- Functional expression.
                xF'             <- downArgX xF

                -- Arguments and theit ypes.
                xsArg'          <- mapM downArgX xsArgs
                tsArg'          <- mapM (convertValueT defs kenv) tsArg

                -- Result and its type.
                tResult'        <- convertValueT defs kenv tResult
                let tPrimeResult' = fromMaybe A.rTop $ takePrimeRegion tResult'

                -- Evaluate a thunk, returning the resulting Addr#, 
                -- then cast it back to a pointer of the appropriate type
                return  $ xMakePtr a tPrimeResult' A.tObj
                        $ A.xApplyThunk a nArgs 
                        $   [ xTakePtr a A.rTop A.tObj xF' ]
                         ++ [ xTakePtr a tPrime A.tObj xArg'
                                | xArg'         <- xsArg'
                                | tArg'         <- tsArg'
                                , let tPrime    = fromMaybe A.rTop
                                                $ takePrimeRegion tArg' ]


        ---------------------------------------------------
        -- This isn't a call primitive.
        _ -> Nothing

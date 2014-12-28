
module DDC.Core.Tetra.Convert.Exp.PrimCall
        (convertPrimCall)
where
import DDC.Core.Tetra.Convert.Exp.Arg
import DDC.Core.Tetra.Convert.Exp.Base
import DDC.Core.Tetra.Convert.Type
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Base.Pretty
import DDC.Base.Panic
import DDC.Core.Check                    (AnTEC(..))
import qualified Data.Map                as Map
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
         , Just (aF, xF_super, arity, atsArg)
            <- case xF of
                XVar aF uF@(UName nF)
                 -- This variable was let-bound to the application of a super
                 -- name to some type arguments, like f = g [t1] [t2]. 
                 -- The value arity and extra type arguments are stashed in the
                 -- ConvertM state monad. See [Note: Binding top-level supers]
                 --
                 -- TODO: check this works with repeated bindings,
                 --       like f  = g1 [t1] [t2]
                 --            g1 = g2 [t3] [t4] [t5]
                 |  Just (nSuper, atsArgs) 
                        <- Map.lookup nF (contextSuperBinds ctx) 
                 -> let 
                        -- If this fails then the super name is in-scope, but
                        -- we can't see its definition in this module, or
                        -- salt-level import to get the arity.
                        arity   = fromMaybe (panicNoArity (UName nSuper) xx)
                                $ superDataArity ctx (UName nSuper)

                        xF'     = XVar aF (UName nSuper)
                    in  Just (aF, xF', arity, atsArgs)

                 -- The name is that of an existing top-level super.
                 | otherwise
                 -> let 
                        -- If this fails then the super name is in-scope, but
                        -- we can't see its definition in this module, or
                        -- salt-level import to get the arity.
                        arity   = fromMaybe (panicNoArity uF xx) 
                                $ superDataArity ctx uF

                    in  Just (aF, xF, arity, [])

                _ -> Nothing

         -> Just $ do
                xF_super' <- downArgX xF_super
                mxsArgs'  <- mapM (convertOrDiscardSuperArgX xx ctx) 
                          $  [XType aArg tArg | (aArg, tArg) <- atsArg]

                let xsArgs' = catMaybes mxsArgs'
                let xF'     = xApps a xF_super' xsArgs'

                tF'     <- convertSuperT (typeContext ctx) (annotType aF)
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
                tsArg'          <- mapM (convertValueT (typeContext ctx)) tsArg
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

                -- Arguments and their ypes.
                xsArg'          <- mapM downArgX xsArgs
                tsArg'          <- mapM (convertValueT (typeContext ctx)) tsArg

                -- Result and its type.
                tResult'        <- convertValueT (typeContext ctx) tResult
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


-- | Couldn't find the arity of an in-scope super.
panicNoArity :: Show a => Bound E.Name -> Exp (AnTEC a E.Name) E.Name -> b
panicNoArity uF xx
        = panic "ddc-core-tetra" "convertPrimCall" $ vcat
        [ text "Cannot find arity for application of super " <> (squotes $ ppr uF)
        , text " in expression: " <> ppr xx 
        , empty
        , text "The super is allegedly in-scope, but we can't see its definition"
        , text "in this module, nor can we get the arity from an import." ]

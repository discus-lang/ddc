
module DDC.Driver.Command.FlowThread
        (cmdFlowThread)
where
import DDC.Build.Pipeline
import DDC.Build.Language.Flow
import DDC.Driver.Stage
import DDC.Driver.Source
import DDC.Core.Exp
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Env
import DDC.Core.Flow.Compounds
import DDC.Data.Canned
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import qualified DDC.Core.Transform.Thread      as Thread
import qualified DDC.Base.Pretty                as P


-- | Thread a state token through the given flow program.
--     This can't be generic in the language fragment because we
--     need to provide a specific type to use for the world token,
--     and new types for the effectful combinators.
cmdFlowThread
        :: Config
        -> Source       -- ^ Source of the code.
        -> String       -- ^ Program module text.
        -> ErrorT String IO ()

cmdFlowThread _config source sourceText
 = do   
        errs    <- liftIO
                $  pipeText (nameOfSource source)
                            (lineStartOfSource source)
                            sourceText
                $  PipeTextLoadCore fragment
                [  PipeCoreStrip
                [  PipeCoreHacks 
                        (Canned $ \m -> return 
                                     $  Thread.thread threadConfig m)
                [  PipeCoreOutput SinkStdout ]]]

        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es


-- | Thread config defines what state token to use,
--   and what functions need to have it threaded though them.
threadConfig :: Thread.Config () Name
threadConfig
        = Thread.Config
        { Thread.configDataDefs         = primDataDefs
        , Thread.configTokenType        = tWorld
        , Thread.configWrapResultType   = wrapResultType
        , Thread.configWrapResultExp    = wrapResultExp
        , Thread.configThreadMe         = newType 
        , Thread.configThreadPat        = unwrapResult }


-- | Get the new type for a stateful primop.
--   The new type contains the World# argument and result.
newType :: Name -> Maybe (Type Name)
newType n
 = case n of
        -- new#  :: [a : Data]. a -> World# -> T2# (World#, Ref# a)
        NameOpStore OpStoreNew
         -> Just $ tForall kData 
                 $ \tA -> tA `tFunPE` tWorld 
                        `tFunPE` (tTuple2 tWorld (tRef tA))

        -- read# :: [a : Data]. Ref# a -> World# -> T2# (World#, a)
        NameOpStore OpStoreRead
         -> Just $ tForall kData
                 $ \tA -> tRef tA `tFunPE` tWorld 
                        `tFunPE` (tTuple2 tWorld (tRef tA))

        -- write# :: [a : Data]. Ref# -> a -> T2# (World#, Unit)
        NameOpStore OpStoreWrite 
         -> Just $ tForall kData
                 $ \tA -> tRef tA `tFunPE` tA `tFunPE` tWorld
                        `tFunPE` (tTuple2 tWorld tUnit)

        -- next#  :: [k : Rate]. [a : Data]
        --        .  Stream# k a -> Int# -> World# -> (World#, a)
        NameOpStore OpStoreNext
         -> Just $ tForalls [kRate, kData]
                 $ \[tK, tA] -> tStream tK tA `tFunPE` tInt `tFunPE` tWorld
                             `tFunPE` (tTuple2 tWorld tA)

        -- loop#  :: Nat# -> (Nat# -> World# -> (World#, Unit)) 
        --        -> World# -> T2# (World#, Unit)
        NameOpLoop  OpLoopLoop
         -> Just $ tNat 
                `tFunPE` (tNat `tFunPE` tWorld `tFunPE` tTuple2 tWorld tNat) 
                `tFunPE` tWorld
                `tFunPE` (tTuple2 tWorld tUnit)

        _ -> Nothing


-- | Wrap the result type of a stateful computation with the state type.
wrapResultType :: Type Name -> Type Name
wrapResultType tt
 = tTuple2 tWorld tt


-- | Wrap the result of a stateful computation with the state token.
wrapResultExp  :: Exp () Name -> Exp () Name -> Exp () Name
wrapResultExp xWorld xResult
 = xTuple2 () xWorld xResult


-- | Make a pattern to unwrap the result of a stateful computation.
unwrapResult   :: Name -> Maybe (Bind Name -> Bind Name -> Pat Name)
unwrapResult _
 = Just unwrap
 where  unwrap bWorld bResult 
         = PData dcTuple2 [bWorld, bResult]



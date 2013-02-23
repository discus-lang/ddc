
-- | The `Flow` fragment is used for data-flow optimisation as part
--   of the Data Parallel Haskell vectorisation pipeline.
module DDC.Build.Language.Flow
        ( language
        , bundle
        , fragment
        , freshT
        , freshX)
where
import DDC.Build.Language.Base
import DDC.Core.Simplifier
import DDC.Core.Transform.Namify
import DDC.Core.Fragment                hiding (Error(..))
import DDC.Core.Flow                    as Flow
import DDC.Type.Exp
import DDC.Type.Env                     (Env)
import DDC.Base.Pretty
import qualified DDC.Type.Env           as Env
import qualified Data.Map               as Map
import Control.Monad.State.Strict


-- | Language definition for Disciple Core Lite.
language    :: Language
language    = Language bundle


-- | Language bundle for Disciple Core Lite.
bundle  :: Bundle Int Name Error
bundle
        = Bundle
        { bundleFragment        = fragment
        , bundleModules         = Map.empty
        , bundleStateInit       = 0 :: Int
        , bundleSimplifier      = Trans Id
        , bundleMakeNamifierT   = makeNamifier freshT 
        , bundleMakeNamifierX   = makeNamifier freshX 
        , bundleRewriteRules    = Map.empty }


-- | Fragement definition for Disciple Core Lite.
fragment :: Fragment Name Error
fragment
        = Fragment
        { fragmentProfile       = profile 
        , fragmentExtension     = "dcf"
        , fragmentReadName      = readName
        , fragmentLexModule     = lexModuleString
        , fragmentLexExp        = lexExpString
        , fragmentCheckModule   = const Nothing
        , fragmentCheckExp      = const Nothing }


data Error a
        = Error
        deriving Show

instance Pretty (Error a) where
 ppr Error  = text (show Error)


-- | Create a new type variable name that is not in the given environment.
freshT :: Env Name -> Bind Name -> State Int Name
freshT env bb
 = do   i       <- get
        put (i + 1)
        let n =  NameVar ("t" ++ show i)
        case Env.lookupName n env of
         Nothing -> return n
         _       -> freshT env bb


-- | Create a new value variable name that is not in the given environment.
freshX :: Env Name -> Bind Name -> State Int Name
freshX env bb
 = do   i       <- get
        put (i + 1)
        let n = NameVar ("x" ++ show i)
        case Env.lookupName n env of
         Nothing -> return n
         _       -> freshX env bb

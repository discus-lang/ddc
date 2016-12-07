
-- | The `Machine` fragment is used for data-flow optimisation as part
--   of the Data Parallel Haskell vectorisation pipeline.
module DDC.Build.Language.Machine
        ( language
        , bundle
        , fragment

        , Error (..))
where
import DDC.Build.Language.Base
import DDC.Core.Simplifier
import DDC.Core.Transform.Namify
import DDC.Core.Fragment                hiding (Error(..))
import DDC.Core.Machine                 as Machine
import DDC.Data.Pretty
import qualified Data.Map               as Map

import DDC.Type.Exp
import DDC.Type.Env                     (Env)
import qualified DDC.Type.Env           as Env
import Control.Monad.State.Strict


-- | Language definition for Disciple Core Machine.
language    :: Language
language    = Language bundle


-- | Language bundle for Disciple Core Machine.
bundle  :: Bundle Int Name Error
bundle  = Bundle
        { bundleFragment        = fragment
        , bundleModules         = Map.empty
        , bundleStateInit       = 0 :: Int
        , bundleSimplifier      = Trans Id
        , bundleMakeNamifierT   = makeNamifier freshT 
        , bundleMakeNamifierX   = makeNamifier freshX 
        , bundleRewriteRules    = Map.empty }


-- | Fragement definition for Disciple Core Machine.
fragment :: Fragment Name Error
fragment
        = Fragment
        { fragmentProfile       = profile 
        , fragmentExtension     = "dcm"
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
        let n =  NameVar $ "t" ++ show i
        case Env.lookupName n env of
         Nothing -> return n
         _       -> freshT env bb


-- | Create a new value variable name that is not in the given environment.
freshX :: Env Name -> Bind Name -> State Int Name
freshX env bb
 = do   i       <- get
        put (i + 1)
        let n = NameVar $ "x" ++ show i
        case Env.lookupName n env of
         Nothing -> return n
         _       -> freshX env bb


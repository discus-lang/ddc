
module DDC.Build.Language.Eval
        ( language
        , bundle
        , fragment
        , profile)
where
import DDC.Build.Language.Base
import DDC.Core.Simplifier
import DDC.Core.Transform.Namify
import DDC.Core.Eval.Profile
import DDC.Core.Eval.Name
import DDC.Core.Fragment
import DDC.Core.Eval.Check                      as Eval
import DDC.Type.Exp
import DDC.Type.Env                             (Env)
import qualified DDC.Type.Env                   as Env
import qualified Data.Map                       as Map
import Control.Monad.State.Strict


-- | Language definition for Disciple Core Eval.
language    :: Language
language    = Language bundle


-- | Language bundle for Disciple Core Eval.
bundle      :: Bundle Int Name Eval.Error
bundle  = Bundle
        { bundleFragment        = fragment
        , bundleModules         = Map.empty
        , bundleStateInit       = 0 :: Int
        , bundleSimplifier      = Trans Id
        , bundleMakeNamifierT   = makeNamifier freshT
        , bundleMakeNamifierX   = makeNamifier freshX
        , bundleRewriteRules    = Map.empty }


-- | Fragment definition for Disciple Core Eval.
fragment :: Fragment Name Eval.Error
fragment
        = Fragment
        { fragmentProfile       = evalProfile
        , fragmentExtension     = "dcv"
        , fragmentReadName      = readName
        , fragmentLexModule     = lexModuleString
        , fragmentLexExp        = lexExpString
        , fragmentCheckModule   = checkCapsModule
        , fragmentCheckExp      = checkCapsX  }


profile = evalProfile

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
        let n = NameVar ("v" ++ show i)
        case Env.lookupName n env of
         Nothing -> return n
         _       -> freshX env bb


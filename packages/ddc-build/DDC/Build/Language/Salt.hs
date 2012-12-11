
-- | The `Salt` fragment contains just those features that can be easily mapped
--   onto C or LLVM code.
module DDC.Build.Language.Salt
        ( language
        , bundle
        , fragment
        , freshT
        , freshX)
where
import DDC.Build.Language.Base
import DDC.Core.Simplifier
import DDC.Core.Transform.Namify
import DDC.Core.Fragment
import DDC.Core.Salt                    as Salt
import DDC.Type.Exp
import DDC.Type.Env                     (Env)
import qualified DDC.Type.Env           as Env
import qualified Data.Map               as Map
import Control.Monad.State.Strict


-- | Language definition for Disciple Core Salt.
language :: Language
language = Language bundle


-- | Language bundle for Disciple Core Salt.
bundle   :: Bundle Int Name Salt.Error
bundle  = Bundle
        { bundleFragment        = fragment
        , bundleModules         = Map.empty
        , bundleStateInit       = 0 :: Int
        , bundleSimplifier      = Trans Id
        , bundleMakeNamifierT   = makeNamifier freshT 
        , bundleMakeNamifierX   = makeNamifier freshX
        , bundleRewriteRules    = Map.empty }


-- | Fragment definition for Disciple Core Salt.
fragment :: Fragment Name Salt.Error
fragment 
        = Fragment
        { fragmentProfile       = profile 
        , fragmentExtension     = "dcs"
        , fragmentLexModule     = lexModuleString
        , fragmentLexExp        = lexExpString
        , fragmentCheckModule   = const Nothing
        , fragmentCheckExp      = const Nothing }


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

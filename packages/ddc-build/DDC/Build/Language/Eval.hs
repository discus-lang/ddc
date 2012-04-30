
module DDC.Build.Language.Eval
        (fragmentEval)
where
import DDC.Build.Language.Base
import DDC.Core.Eval.Profile
import DDC.Core.Eval.Name
import DDC.Core.Eval.Check
import DDC.Core.Transform.Namify
import DDC.Type.Exp
import DDC.Type.Env                             (Env)
import qualified DDC.Type.Env                   as Env
import Control.Monad.State.Strict


fragmentEval :: Fragment Name Error
fragmentEval
        = Fragment
        { fragmentProfile       = evalProfile
        , fragmentLexModule     = lexModuleString
        , fragmentLexExp        = lexExpString
        , fragmentCheckModule   = error "DDC.Core.Language.Eval: finish me"
        , fragmentCheckExp      = checkCapsX 
        , fragmentMakeNamifierT = makeNamifier freshT
        , fragmentMakeNamifierX = makeNamifier freshX 
        , fragmentNameZero      = 0 :: Int }


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


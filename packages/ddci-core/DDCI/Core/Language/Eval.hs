
module DDCI.Core.Language.Eval
        (fragmentEval)
where
import DDCI.Core.Mode
import DDCI.Core.Language.Base
import Control.Monad.State.Strict
import DDC.Type.Env                             (Env)
import DDC.Type.Exp
import DDC.Core.Eval.Profile
import DDC.Core.Eval.Name
import DDC.Core.Eval.Check
import DDC.Core.Transform.Namify
import qualified DDC.Type.Env                   as Env


fragmentEval :: Fragment Name Error
fragmentEval
        = Fragment
        { fragmentProfile       = evalProfile

        , fragmentLexModule
                = \s str -> lexModuleString (nameOfSource s) (lineStartOfSource s) str

        , fragmentLexExp        
                = \s str -> lexExpString    (nameOfSource s) (lineStartOfSource s) str

        , fragmentCheckModule   = error "languages: finish me"
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


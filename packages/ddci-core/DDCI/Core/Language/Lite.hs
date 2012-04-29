
module DDCI.Core.Language.Lite
        (fragmentLite)
where
import DDCI.Core.Mode
import DDCI.Core.Language.Base
import DDC.Core.Transform.Namify
import DDC.Type.Exp
import Control.Monad.State.Strict
import DDC.Type.Env                     (Env)
import DDC.Core.Brine.Lite
import qualified DDC.Type.Env           as Env


fragmentLite :: Fragment Name Error
fragmentLite
        = Fragment
        { fragmentProfile       = profile 

        , fragmentLexModule
                = \s str -> lexModuleString (nameOfSource s) (lineStartOfSource s) str

        , fragmentLexExp
                = \s str -> lexExpString    (nameOfSource s) (lineStartOfSource s) str

        , fragmentCheckModule   = const Nothing
        , fragmentCheckExp      = const Nothing
        , fragmentMakeNamifierT = makeNamifier freshT 
        , fragmentMakeNamifierX = makeNamifier freshX 
        , fragmentNameZero      = (0 :: Int) }



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

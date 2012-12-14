
-- | The `Zero` fragment has no features and no primops.
--   It it provides the first order calculus, and is good for debugging.
module DDC.Build.Language.Zero
        ( language
        , bundle
        , fragment
        , Name
        , Error)
where
import DDC.Core.Simplifier
import DDC.Build.Language.Base
import DDC.Core.Fragment                hiding (Error)
import DDC.Core.Transform.Namify
import DDC.Base.Pretty
import DDC.Data.Token
import DDC.Type.Exp
import Data.Typeable
import DDC.Type.Env                     (Env)
import DDC.Core.Lexer                   as Core
import qualified DDC.Type.Env           as Env
import qualified Data.Map               as Map
import Control.Monad.State.Strict
import Control.DeepSeq


-- | Language definitition for Disciple Core Zero.
language :: Language
language = Language bundle


-- | Language bundle for Disciple Core Zero
bundle      :: Bundle Int Name Error
bundle  = Bundle
        { bundleFragment        = fragment
        , bundleModules         = Map.empty
        , bundleStateInit       = 0 :: Int
        , bundleSimplifier      = Trans Id
        , bundleMakeNamifierT   = makeNamifier freshT
        , bundleMakeNamifierX   = makeNamifier freshX
        , bundleRewriteRules    = Map.empty }


-- | Fragment definition for Disciple Core Eval.
fragment :: Fragment Name Error
fragment
        = Fragment
        { fragmentProfile       = zeroProfile
        , fragmentExtension     = "dcz"
        , fragmentReadName      = \x -> Just (Name x)
        , fragmentLexModule     = lexModuleZero
        , fragmentLexExp        = lexExpZero
        , fragmentCheckModule   = const Nothing
        , fragmentCheckExp      = const Nothing }


data Error a
        = Error
        deriving Show

instance Pretty (Error a) where
 ppr Error  = text (show Error)


-- Wrap the names we use for the zero fragment, 
-- so they get pretty printed properly.
data Name 
        = Name String
        deriving (Eq, Ord, Show, Typeable)

instance NFData Name where
 rnf (Name str) = rnf str

instance Pretty Name where
 ppr (Name str) = text str


-- | Lex a string to tokens, using primitive names.
--
--   The first argument gives the starting source line number.
lexModuleZero :: String -> Int -> String -> [Token (Tok Name)]
lexModuleZero srcName srcLine str
 = map rn $ Core.lexModuleWithOffside srcName srcLine str
 where rn (Token t sp) 
        = case renameTok (Just . Name) t of
                Just t' -> Token t' sp
                Nothing -> Token (KJunk "lexical error") sp


-- | Lex a string to tokens, using primitive names.
--
--   The first argument gives the starting source line number.
lexExpZero :: String -> Int -> String -> [Token (Tok Name)]
lexExpZero srcName srcLine str
 = map rn $ Core.lexExp srcName srcLine str
 where rn (Token t sp) 
        = case renameTok (Just . Name) t of
                Just t' -> Token t' sp
                Nothing -> Token (KJunk "lexical error") sp


-- | Create a new type variable name that is not in the given environment.
freshT :: Env Name -> Bind Name -> State Int Name
freshT env bb
 = do   i       <- get
        put (i + 1)
        let n =  Name $ "t" ++ show i
        case Env.lookupName n env of
         Nothing -> return n
         _       -> freshT env bb


-- | Create a new value variable name that is not in the given environment.
freshX :: Env Name -> Bind Name -> State Int Name
freshX env bb
 = do   i       <- get
        put (i + 1)
        let n = Name $ "x" ++ show i
        case Env.lookupName n env of
         Nothing -> return n
         _       -> freshX env bb

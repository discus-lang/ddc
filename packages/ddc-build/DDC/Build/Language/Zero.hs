
module DDC.Build.Language.Zero
        ( fragmentZero
        , Name
        , Error)
where
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
import Control.Monad.State.Strict


-- | The `Zero` fragment has no features and no primops.
--   It it provides the first order calculus, and is good for debugging.
fragmentZero :: Fragment Name Error
fragmentZero 
        = Fragment
        { fragmentProfile       = (zeroProfile :: Profile Name)
        , fragmentExtension     = "dcz"
        , fragmentLexModule     = lexModuleZero
        , fragmentLexExp        = lexExpZero
        , fragmentCheckModule   = const Nothing
        , fragmentCheckExp      = const Nothing 
        , fragmentMakeNamifierT = makeNamifier freshT
        , fragmentMakeNamifierX = makeNamifier freshX 
        , fragmentNameZero      = 0 :: Int }


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

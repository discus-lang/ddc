
-- | Language profile for the SeaOutput fragment of Disciple Core.
module DDC.Core.Sea.Output.Profile
        ( profile
        , lexString)
where
import DDC.Core.Parser.Lexer
import DDC.Core.Language.Profile
import DDC.Core.Sea.Output.Env
import DDC.Core.Sea.Output.Name
import Data.Maybe
import DDC.Base.Lexer
import DDC.Core.Parser.Tokens


-- | Profile for the Sea Output fragment of the core language. 
--
--   This can be converted directly to C and LLVM code.
profile :: Profile Name 
profile
        = Profile
        { profileName           = "Output"
        , profileFeatures       = features
        , profilePrimDataDefs   = primDataDefs
        , profilePrimKinds      = primKindEnv
        , profilePrimTypes      = primTypeEnv }


-- | The SeaOutput fragment doesn't support many features of Disciple Core.
features :: Features
features = zeroFeatures


-- | Lex a string to tokens, using primitive names.
--
--   The first argument gives the starting source line number.
lexString :: Int -> String -> [Token (Tok Name)]
lexString lineStart str
 = map rn $ lexExp lineStart str
 where rn (Token t sp) = Token (renameTok readName_ t) sp

       readName_ str'
        = fromMaybe (error $ "DDC.Core.Sea.Output.Profile: unknown name " ++ show str')
        $ readName str'

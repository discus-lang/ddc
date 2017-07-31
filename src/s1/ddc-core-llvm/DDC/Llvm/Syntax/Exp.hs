
module DDC.Llvm.Syntax.Exp
        ( -- * Expressions
          Exp   (..)
        , typeOfExp
        , isXVar, isXLit, isXUndef
        , isClosedConstantExp

          -- * Variables
        , Var   (..)
        , nameOfVar
        , typeOfVar

          -- * Names
        , Name  (..)
        , textOfName

          -- * Literals
        , Lit   (..)
        , typeOfLit
        , makeLitString)
where
import DDC.Llvm.Syntax.Type
import DDC.Llvm.Syntax.Prim
import DDC.Llvm.Pretty.Prim             ()
import Data.Text                        (Text)
import Data.Char
import Numeric
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.ByteString        as BS


-- Exp ------------------------------------------------------------------------
-- | Expressions can be used directly as arguments to instructions.
--
--   The expressions marked (synthetic) are safe conversions that do not
--   branch or access memory. In the real LLVM syntax we cannot represent
--   them as expressions, but they are flattened out to instructions by the
--   Clean transform.
--
data Exp
        -- | Use of a variable.
        = XVar   Var

        -- | A literal.
        | XLit   Lit

        -- | An undefined value.
        | XUndef Type

        -- | (synthetic) Cast an expression to the given type.
        | XConv  Type Conv Exp

        -- | (synthetic) Get a pointer to an element of the expression.
        | XGet   Type Exp [Exp]
        deriving (Eq, Show)


-- | Take the type of an expression.
typeOfExp :: Exp -> Type
typeOfExp xx
 = case xx of
        XVar   var      -> typeOfVar var
        XLit   lit      -> typeOfLit lit
        XUndef t        -> t

        XConv  t _ _    -> t
        XGet   t _ _    -> t


-- | Check if this expression is an `XVar`.
isXVar :: Exp -> Bool
isXVar xx
 = case xx of
        XVar{}  -> True
        _       -> False


-- | Check if this expression is an `XLit`.
isXLit :: Exp -> Bool
isXLit xx
 = case xx of
        XLit{}  -> True
        _       -> False


-- | Check if this expression is an `XUndef`.
isXUndef :: Exp -> Bool
isXUndef xx
 = case xx of
        XUndef{} -> True
        _        -> False


-- | Check whether this expression is closed,
--   meaning it doesn't contain any variables that refer to the context.
isClosedConstantExp :: Exp -> Bool
isClosedConstantExp xx
 = case xx of
        XVar{}          -> False
        XLit{}          -> True
        XUndef{}        -> True
        XConv _ _ x     -> isClosedConstantExp x
        XGet  _ x1 xs   -> isClosedConstantExp x1 && all isClosedConstantExp xs


-- Var ------------------------------------------------------------------------
-- | A variable that can be assigned to.
data Var
        = Var   Name    Type
        deriving (Eq, Show)


-- | Yield the name of a var.
nameOfVar :: Var -> Name
nameOfVar (Var n _)     = n


-- | Yield the type of a var.
typeOfVar :: Var -> Type
typeOfVar (Var _ t)     = t


instance Ord Var where
 compare (Var n1 _) (Var n2 _)
        = compare n1 n2


-- Name -----------------------------------------------------------------------
-- | Names of variables.
data Name
        = NameGlobal String
        | NameLocal  String
        deriving (Show, Eq, Ord)


-- | Yield a name as a string.
textOfName :: Name -> String
textOfName (NameGlobal str) = str
textOfName (NameLocal  str) = str


-- Lit ------------------------------------------------------------------------
-- | Literal data.
data Lit
        -- | An integer literal
        = LitInt        Type    Integer

        -- | A floating-point literal.
        | LitFloat      Type    Double

        -- | A string literal.
        --   In LLVM these have the same type as array literals, but have a
        --   special syntax. The first component is the literal source text,
        --   while the second its the pretty printed hex encoding that
        --   the LLVM frontend accepts.
        | LitString
        { litSource             :: Text
        , litHexEncoded         :: Text
        , litEncodingLength     :: Int }

        -- | A null pointer literal.
        --   Only applicable to pointer types
        | LitNull       Type

        -- | A completely undefined value.
        | LitUndef      Type
        deriving (Eq, Show)


-- | Yield the `Type` of a `Lit`.
typeOfLit :: Lit -> Type
typeOfLit ll
 = case ll of
        LitInt    t _   -> t
        LitFloat  t _   -> t
        LitNull   t     -> t
        LitUndef  t     -> t

        LitString _ _ encLen
         -> TArray (fromIntegral encLen) (TInt 8)



-- | Make a literal string from some text.
makeLitString :: Text -> Lit
makeLitString tx
 = let  (txEnc, nEncLen) = encodeText (tx `T.append` (T.pack [chr 0]))
   in   LitString tx txEnc nEncLen


-- | Hex encode non-printable characters in this string.
--   The LLVM frontend doesn't appear to be unicode-clean, so only unoffensive
--   ASCII characters are printed verbatim. Everything is hex-encoded as UTF-8.
encodeText :: Text -> (Text, Int)
encodeText tx
 = go [] 0 tx
 where
        go accStr accLen xx
         = case T.uncons xx of
             Nothing
              -> (T.concat $ reverse accStr, accLen)

             Just (x, xs)
              -> let (str, len) = encodeChar x
                 in  go (str : accStr) (accLen + len) xs

        encodeChar c
         | c == ' '
          || (isAscii c && isAlphaNum c)
          || (isAscii c && isPunctuation c && c /= '"' && c /= '\\')
         = (T.pack [c], 1)

         | otherwise
         = let  bs      = TE.encodeUtf8 $ T.pack [c]
                len     = BS.length bs
           in   ( T.pack $ concatMap (\b -> "\\" ++ (padL $ showHex b ""))
                         $ BS.unpack bs
                , len)

        padL x
         | length x == 0  = "00"
         | length x == 1  = "0"  ++ x
         | otherwise      = x


{-# LANGUAGE TypeFamilies #-}

-- | Pretty printer utilities.
--
--   This is a re-export of Daan Leijen's pretty printer package (@wl-pprint@),
--   but with a `Pretty` class that includes a `pprPrec` function.
module DDC.Data.Pretty
        ( Pretty(..)
        , P.Doc

        -- * Character documents
        , P.lparen,   P.rparen
        , P.langle,   P.rangle
        , P.lbrace,   P.rbrace
        , P.lbracket, P.rbracket
        , P.squote,   P.dquote
        , P.semi, P.colon, P.comma, P.space, P.dot, P.backslash, P.equals

        -- * Primitive type documents.
        , P.char, P.string, P.text, P.int, P.integer, P.float, P.double

        -- * Basic combinators
        , P.line,  P.linebreak, P.softline, P.softbreak
        , P.nest,  P.align, P.hang, P.indent

        , P.pspace, P.pline, P.plinebreak, P.psoftline, P.psoftbreak

        -- * Bracketing combinators
        , P.enclose, P.parens, P.squotes, P.dquotes, P.angles, P.braces, P.brackets
        , P.encloseSep, P.tupled

        -- * List combinators
        , P.sep, P.hsep, P.vsep
        , P.cat, P.hcat, P.vcat
        , P.punctuate

        -- * Operators
        , (<+>), (</>)

        , pprParen
        , padL
        , P.fill, P.width

        -- * Rendering
        , RenderMode (..)
        , render
        , renderPlain
        , renderIndent
        , putDoc, putDocLn)
where
import Data.Set                         (Set)
import qualified Data.Set               as Set
import qualified DDC.Data.PrettyPrint   as P


-- Utils ---------------------------------------------------------------------
-- | Wrap a `Doc` in parens if the predicate is true.
pprParen :: Bool -> P.Doc -> P.Doc
pprParen b c
 = if b then P.parens c
        else c


infixr 5 </>
infixr 7 <+>

(<+>)   :: P.Doc -> P.Doc -> P.Doc
(<+>)   =  P.pspace

(</>)   :: P.Doc -> P.Doc -> P.Doc
(</>)   =  P.psoftline



-- Pretty Class --------------------------------------------------------------
class Pretty a where
 data PrettyMode a
 pprDefaultMode :: PrettyMode a

 ppr            :: a   -> P.Doc
 ppr            = pprPrec 0

 pprPrec        :: Int -> a -> P.Doc
 pprPrec p      = pprModePrec pprDefaultMode p

 pprModePrec    :: PrettyMode a -> Int -> a -> P.Doc
 pprModePrec _ _ x = ppr x


instance Pretty () where
 ppr    = P.text . show

instance Pretty Bool where
 ppr    = P.text . show

instance Pretty Int where
 ppr    = P.text . show

instance Pretty Integer where
 ppr    = P.text . show

instance Pretty Char where
 ppr    = P.text . show

instance Pretty a => Pretty [a] where
 ppr xs  = P.encloseSep P.lbracket P.rbracket P.comma
         $ map ppr xs

instance Pretty a => Pretty (Set a) where
 ppr xs  = P.encloseSep P.lbracket P.rbracket P.comma
         $ map ppr $ Set.toList xs

instance (Pretty a, Pretty b) => Pretty (a, b) where
 ppr (a, b) = P.parens $ ppr a <> P.comma <> ppr b


padL :: Int -> P.Doc -> P.Doc
padL n d
 = let  len     = length $ renderPlain d
        pad     = n - len
   in   if pad > 0
         then  d <> P.text (replicate pad ' ')
         else  d


-- Rendering ------------------------------------------------------------------
-- | How to pretty print a doc.
data RenderMode
        -- | Render the doc with indenting.
        = RenderPlain

        -- | Render the doc without indenting.
        | RenderIndent
        deriving (Eq, Show)


-- | Render a doc with the given mode.
render :: RenderMode -> P.Doc -> String
render mode doc
 = case mode of
        RenderPlain  -> eatSpace True $ P.displayS (P.renderCompact doc) ""
        RenderIndent -> P.displayS (P.renderPretty 0.8 100000 doc) ""

 where  eatSpace :: Bool -> String -> String
        eatSpace _    []        = []
        eatSpace True (c:cs)
         = case c of
                ' '     -> eatSpace True cs
                '\n'    -> eatSpace True cs
                _       -> c   : eatSpace False cs

        eatSpace False (c:cs)
         = case c of
                ' '     -> ' ' : eatSpace True cs
                '\n'    -> ' ' : eatSpace True cs
                _       -> c   : eatSpace False cs


-- | Convert a `Doc` to a string without indentation.
renderPlain  :: P.Doc -> String
renderPlain = render RenderPlain


-- | Convert a `Doc` to a string with indentation
renderIndent :: P.Doc -> String
renderIndent = render RenderIndent


-- | Put a `Doc` to `stdout` using the given mode.
putDoc :: RenderMode -> P.Doc -> IO ()
putDoc mode doc
        = putStr   $ render mode doc

-- | Put a `Doc` to `stdout` using the given mode.
putDocLn  :: RenderMode -> P.Doc -> IO ()
putDocLn mode doc
        = putStrLn $ render mode doc




-- This module is a minimal replacement for a module in the code base of the
-- GHC compiler. This is needed to allow the use of David Terei's LLVM output
-- code that is part of GHC.

module Llvm.GhcReplace.Pretty where

import DDC.Main.Pretty
import DDC.Util.Pretty.Base
import DDC.Util.Pretty.Combinators()

type Doc = StrMode [PrettyMode]

infixl 6 <>
infixl 6 <+>
infixl 5 $$, $+$

(<+>)  :: Doc -> Doc -> Doc     -- Beside, separated by space
(<+>) a b = a %% b

(<>)   :: Doc -> Doc -> Doc     -- Beside
(<>) a b = a % b

($$)   :: Doc -> Doc -> Doc     -- Above; if there is no
                                -- overlap it "dovetails" the two
($$) = prettyLineJoin

($+$) :: Doc -> Doc -> Doc
($+$) a b = a % nl % b

text :: String -> Doc
text s = ppr s

empty, lparen, rparen, space, equals :: Doc
empty = PBlank
space = PChar ' '
rparen = PChar ')'
lparen = PChar '('
equals = PChar '='

lbrace, rbrace :: Doc
lbrace = PChar '{'
rbrace = PChar '}'

doubleQuotes :: Doc -> Doc
doubleQuotes = dquotes

ftext :: String -> Doc
ftext fs = ppr fs

hcat :: [Doc] -> Doc          -- List version of <>
hcat = foldr (<>)  empty

vcat :: [Doc] -> Doc          -- List version of $$
vcat = foldr ($$)  empty

int :: Int -> Doc
int i = ppr i

nest :: Doc -> Doc
nest s = indent s

--------------------------------------------------------------------------------
-- The following weren't in the original Llvm.GhcReplace.Pretty module.

prettyLineJoin :: Doc -> Doc -> Doc
prettyLineJoin PBlank PBlank = PBlank
prettyLineJoin a PBlank = a
prettyLineJoin PBlank b = b
prettyLineJoin a b = a % nl % b

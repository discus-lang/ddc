-- This module is a minimal replacement for a module in the code base of the
-- GHC compiler. This is needed to allow the use of David Terei's LLVM output
-- code that is part of GHC.

module Llvm.GhcReplace.Pretty where

import Llvm.GhcReplace.FastString

type Doc = String

infixl 6 <>
infixl 6 <+>
infixl 5 $$, $+$

(<+>)  :: Doc -> Doc -> Doc     -- Beside, separated by space
(<+>) a b = a ++ " " ++ b

(<>)   :: Doc -> Doc -> Doc     -- Beside
(<>) a b = a ++ b

($$)   :: Doc -> Doc -> Doc     -- Above; if there is no
                                -- overlap it "dovetails" the two
($$) = prettyLineJoin

($+$) :: Doc -> Doc -> Doc
($+$) = prettyLineJoin

text :: String -> Doc
text = id

empty, lparen, rparen, space, equals :: Doc
empty = ""
space = " "
rparen = ")"
lparen = "("
equals = "="

semi, colon, comma, lbrace, rbrace :: Doc
semi  = ";"
colon = ":"
comma = ","
lbrace = "{"
rbrace = "}"


doubleQuotes :: Doc -> Doc
doubleQuotes p  = "\"" <> p <> "\""

brackets :: Doc -> Doc
brackets p = "[" <> p <> "]"

ftext :: FastString -> Doc
ftext fs = fs

hcat :: [Doc] -> Doc          -- List version of <>
hcat = foldr (<>)  empty

vcat :: [Doc] -> Doc          -- List version of $$
vcat = foldr ($$)  empty

int :: Int -> Doc
int i = show i

nest :: Int -> Doc -> Doc
nest n s
 =  let indent = replicate n ' '
    in unlines
         $ map (indent ++)
         $ filter (\l -> length l > 0)
         $ lines s

--------------------------------------------------------------------------------
-- The following weren't in the original Llvm.GhcReplace.Pretty module.

prettyLineJoin :: Doc -> Doc -> Doc
prettyLineJoin a b
 = case (length a > 0, length b > 0) of
     (True, True) -> a ++ "\n" ++ b
     (True, False) -> a
     (False, True) -> b
     (False, False) -> ""


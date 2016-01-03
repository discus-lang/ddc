
-- | Defines allowable unicode operator symbols.
--  
--   We want to allow the use of common operator symbols that most people
--   can pronounce, but deny the ones that can be confused with others. 
--
--   NOTE: We also want to guide client programmers into using unicode
--   symbols in a sane and friendly way. When we add operator definitions,
--   setup the syntax so that each operator is naturally given a pronouncable
--   name.
--
--    operator compose ∘    as infix 5
--    operator union   ∪    as infix 3
--    operator sqrt    √    as prefix
--    operator and     ∧ /\ as infix 3
--
--   Give up on && and || for logical AND and OR operators.
--   If we allow ∧ and ∨ then the ASCII version should be /\ and \/.
--
--   We could then provide a compiler command to lookup the name and input
--   information for provided operators.
--
module DDC.Core.Lexer.Unicode
        (unicodeOperatorsInfix)
where
import Data.Set                 (Set)
import qualified Data.Set       as Set


-- | Common use of a unicode operator.
data Use
        = Denied
        | Infix
        | Prefix
        deriving Show


-- | Unicode operators that are used infix.
unicodeOperatorsInfix :: Set Char
unicodeOperatorsInfix
        = Set.fromList
        $ [c | (c, _, Infix) <- unicodeOperatorTable]


-- | Symbols from the Unicode Range 2200-22ff "Mathematical Operators".
--   From http://www.unicode.org/charts/PDF/U2200.pdf
--
--   We restrict the allowable unicode to the common ones that most people
--   know how to pronounce, that do not conflict with other symbols, 
--   and that are tradionally used infix.
--
unicodeOperatorTable :: [(Char, String, Use)]
unicodeOperatorTable
 =      [ -- Set membership
          ('∈', "element of",                   Infix)  -- U+2208 ok
        , ('∉', "not an element of",            Infix)  -- U+2209 ok
--      , ('∊', "small element of",             Infix)  -- U+220a looks like U+2208
        , ('∋', "contains as member",           Infix)  -- U+220b
        , ('∌', "does not contain as member",   Infix)  -- U+220c
--      , ('∍', "small contains as member",     Denied) -- U+220d looks like U+220b 

          -- Operators
--        ('−', "minus sign",           Denied)         -- U+2212 looks like regular minus
        , ('∓', "minus-or-plus sign",   Infix)          -- U+2213 ok
        , ('∔', "dot plus",             Infix)          -- U+2214 ok
--      , ('∕', "division slash",       Denied)         -- U+2215 looks like fwd slash.
--      , ('∖', "set minus",            Denied)         -- U+2216 looks like back slash.
--      , ('∗', "asterix operator",     Denied)         -- U+2217 looks like times
        , ('∘', "ring operator",        Infix)          -- U+2218 ok
        , ('∙', "bullet operator",      Infix)          -- U+2219 ok
        , ('√', "square root",          Prefix)         -- U+221a ok
        , ('∛', "cube root",            Prefix)         -- U+221b ok
        , ('∜', "fourth root",          Prefix)         -- U+221c ok
        , ('∝', "proportional to",      Infix)          -- U+221d ok

        -- Logical and set operators.
        , ('∧', "logical and",          Infix)          -- U+2227 ok
        , ('∨', "logical or",           Infix)          -- U+2228 ok
        , ('∩', "intersection",         Infix)          -- U+2229 ok
        , ('∪', "union",                Infix)          -- U+222a ok
        ]

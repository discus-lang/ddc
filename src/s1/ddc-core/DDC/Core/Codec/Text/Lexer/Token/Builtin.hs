
module DDC.Core.Codec.Text.Lexer.Token.Builtin
        ( Builtin (..)
        , sayBuiltin
        , scanBuiltin
        , acceptBuiltin)
where
import DDC.Core.Codec.Text.Lexer.Token.Names
import DDC.Core.Codec.Text.Pretty.Type
import DDC.Core.Exp
import Text.Lexer.Inchworm.Char
import qualified Data.List      as List
import qualified Data.Char      as Char


-------------------------------------------------------------------------------
-- | Builtin name tokens.
data Builtin
        = BSoCon SoCon
        | BKiCon KiCon
        | BTwCon TwCon
        | BTcCon TcCon

        | BPure
        | BEmpty

        | BDaConUnit
        deriving (Eq, Show)


-------------------------------------------------------------------------------
-- | Yield the string name of a Builtin.
sayBuiltin :: Builtin -> String
sayBuiltin bb
 = case bb of
        BSoCon sc       -> renderPlain $ ppr sc
        BKiCon ki       -> renderPlain $ ppr ki
        BTwCon kw       -> renderPlain $ ppr kw
        BTcCon tc       -> renderPlain $ ppr tc
        BPure           -> "Pure"
        BEmpty          -> "Empty"
        BDaConUnit      -> "()"


-------------------------------------------------------------------------------
-- | Scanner for builtin names.
scanBuiltin   :: Scanner IO Location [Char] (Location, Builtin)
scanBuiltin
 = munchPred Nothing matchConName acceptBuiltin


-- | Accept a builtin name.
acceptBuiltin :: String -> Maybe Builtin
acceptBuiltin str
 | Just cc      <- acceptTwConWithArity str
 = Just (BTwCon cc)

acceptBuiltin str
 = case str of
        -- Sort constructors.
        "Prop"          -> Just (BSoCon SoConProp)
        "Comp"          -> Just (BSoCon SoConComp)

        -- Kind constructors.
        "Witness"       -> Just (BKiCon KiConWitness)
        "Data"          -> Just (BKiCon KiConData)
        "Region"        -> Just (BKiCon KiConRegion)
        "Effect"        -> Just (BKiCon KiConEffect)
        "Closure"       -> Just (BKiCon KiConClosure)

        -- Witness type constructors.
        "Const"         -> Just (BTwCon TwConConst)
        "Mutable"       -> Just (BTwCon TwConMutable)
        "Purify"        -> Just (BTwCon TwConPure)
        "Disjoint"      -> Just (BTwCon TwConDisjoint)
        "Distinct"      -> Just (BTwCon (TwConDistinct 2))

        -- Type constructors.
        "Unit"          -> Just (BTcCon TcConUnit)
        "S"             -> Just (BTcCon TcConSusp)
        "T#"            -> Just (BTcCon TcConT)
        "R#"            -> Just (BTcCon TcConR)
        "V#"            -> Just (BTcCon TcConV)
        "Read"          -> Just (BTcCon TcConRead)
        "Write"         -> Just (BTcCon TcConWrite)
        "Alloc"         -> Just (BTcCon TcConAlloc)

        -- Builtin types.
        "Pure"          -> Just BPure
        "Empty"         -> Just BEmpty

        -- Builtin values.
        "()"            -> Just BDaConUnit

        _               -> Nothing


acceptTwConWithArity :: String -> Maybe TwCon
acceptTwConWithArity ss
 | Just n <- List.stripPrefix "Distinct" ss
 , not $ null n
 , all Char.isDigit n
 = Just (TwConDistinct $ read n)

 | otherwise
 = Nothing



module DDC.Core.Lexer.Token.Builtin
        ( Builtin (..)
        , sayBuiltin

        , acceptSoCon
        , acceptKiCon
        , acceptTwCon
        , acceptTcCon)
where
import DDC.Core.Exp
import DDC.Base.Pretty
import qualified Data.List      as List
import qualified Data.Char      as Char


-------------------------------------------------------------------------------
-- | Builtin name tokens.
data Builtin
        = BSoCon        SoCon
        | BKiCon        KiCon
        | BTwCon        TwCon
        | BTcCon        TcCon

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
-- | Accept a named sort constructor.
acceptSoCon :: String -> Maybe SoCon
acceptSoCon ss
 = case ss of
        "Prop"          -> Just SoConProp
        "Comp"          -> Just SoConComp
        _               -> Nothing


-- | Accept a named kind constructor.
acceptKiCon :: String -> Maybe KiCon
acceptKiCon ss
 = case ss of
        "Witness"       -> Just KiConWitness
        "Data"          -> Just KiConData
        "Region"        -> Just KiConRegion
        "Effect"        -> Just KiConEffect
        "Closure"       -> Just KiConClosure
        _               -> Nothing


-- | Read a named witness type constructor.
acceptTwCon :: String -> Maybe TwCon
acceptTwCon ss
 = case ss of
        "Const"         -> Just TwConConst
        "DeepConst"     -> Just TwConDeepConst
        "Mutable"       -> Just TwConMutable
        "DeepMutable"   -> Just TwConDeepMutable
        "Purify"        -> Just TwConPure
        "Disjoint"      -> Just TwConDisjoint
        "Distinct"      -> Just (TwConDistinct 2)
        _               -> acceptTwConWithArity ss


acceptTwConWithArity :: String -> Maybe TwCon
acceptTwConWithArity ss
 | Just n <- List.stripPrefix "Distinct" ss 
 , all Char.isDigit n
 = Just (TwConDistinct $ read n)

 | otherwise
 = Nothing
 
 
-- | Read a builtin type constructor with a non-symbolic name.
--   ie not '->'.
acceptTcCon :: String -> Maybe TcCon
acceptTcCon ss
 = case ss of
        "Unit"          -> Just TcConUnit
        "S"             -> Just TcConSusp
        "Read"          -> Just TcConRead
        "HeadRead"      -> Just TcConHeadRead
        "DeepRead"      -> Just TcConDeepRead
        "Write"         -> Just TcConWrite
        "DeepWrite"     -> Just TcConDeepWrite
        "Alloc"         -> Just TcConAlloc
        "DeepAlloc"     -> Just TcConDeepAlloc
        _               -> Nothing


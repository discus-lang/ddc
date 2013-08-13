
module DDC.Source.Tetra.Desugar.Defix.FixTable
        ( FixTable      (..)
        , FixDef        (..)
        , InfixAssoc    (..)
        , lookupDefInfixOfSymbol
        , lookupDefPrefixOfSymbol
        , getInfixDefOfSymbol
        , defaultFixTable)
where
import DDC.Source.Tetra.Desugar.Defix.Error
import DDC.Source.Tetra.Name
import DDC.Source.Tetra.Exp
import Data.List
import qualified DDC.Data.SourcePos     as BP


-- | Table of infix operator definitions.
data FixTable a n
        = FixTable [FixDef a n]


-- | Infix operator definition.
data FixDef a n
        -- A prefix operator
        = FixDefPrefix
        { -- | String of the operator
          fixDefSymbol  :: String

          -- | Expression to rewrite the operator to, 
          --   given the annotation of the original symbol.
        , fixDefExp     :: a -> Exp a n }

        -- An infix operator.
        | FixDefInfix
        { -- | String of the operator.
          fixDefSymbol  :: String
        
          -- | Expression to rewrite the operator to, 
          --   given the annotation of the original symbol.
        , fixDefExp     :: a -> Exp a n

          -- | Associativity of infix operator.
        , fixDefAssoc   :: InfixAssoc
        
          -- | Precedence of infix operator.
        , fixDefPrec    :: Int }



-- | Infix associativity.
data InfixAssoc
        -- | Left associative.
        --      x * y * z => * (* x y) z
        = InfixLeft

        -- | Right associative.
        --      x * y * z => * x (* y z)
        | InfixRight

        -- | Non associative.
        --      x * y * z => error
        | InfixNone
        deriving (Show, Eq)


-- | Lookup the `FixDefInfix` corresponding to a symbol name, if any.
lookupDefInfixOfSymbol  :: FixTable a n -> String -> Maybe (FixDef a n)
lookupDefInfixOfSymbol (FixTable defs) str
        = find (\def -> case def of
                         FixDefInfix{}  -> fixDefSymbol def == str
                         _              -> False)
                defs


-- | Lookup the `FixDefPrefix` corresponding to a symbol name, if any.
lookupDefPrefixOfSymbol  :: FixTable a n -> String -> Maybe (FixDef a n)
lookupDefPrefixOfSymbol (FixTable defs) str
        = find (\def -> case def of
                         FixDefPrefix{} -> fixDefSymbol def == str
                         _              -> False)
                defs


-- | Get the precedence of an infix symbol, else Error.
getInfixDefOfSymbol 
        :: FixTable a n 
        -> String 
        -> Either (Error a n) (FixDef a n)

getInfixDefOfSymbol table str
 = case lookupDefInfixOfSymbol table str of
        Nothing         -> Left  (ErrorNoInfixDef str)
        Just def        -> Right def


-- | Default fixity table for infix operators.
defaultFixTable :: FixTable BP.SourcePos Name
defaultFixTable
 = FixTable 
        [ FixDefPrefix "-"  (\sp -> XVar sp (UName (NameVar "neg")))
        , FixDefInfix  "*"  (\sp -> XVar sp (UName (NameVar "mul"))) InfixLeft  7
        , FixDefInfix  "+"  (\sp -> XVar sp (UName (NameVar "add"))) InfixLeft  6
        , FixDefInfix  "-"  (\sp -> XVar sp (UName (NameVar "sub"))) InfixLeft  6 
        , FixDefInfix  "==" (\sp -> XVar sp (UName (NameVar "eq" ))) InfixNone  5
        , FixDefInfix  "/=" (\sp -> XVar sp (UName (NameVar "neq"))) InfixNone  5
        , FixDefInfix  "<"  (\sp -> XVar sp (UName (NameVar "lt" ))) InfixNone  5
        , FixDefInfix  "<=" (\sp -> XVar sp (UName (NameVar "le" ))) InfixNone  5
        , FixDefInfix  ">"  (\sp -> XVar sp (UName (NameVar "gt" ))) InfixNone  5
        , FixDefInfix  ">=" (\sp -> XVar sp (UName (NameVar "ge" ))) InfixNone  5
        , FixDefInfix  "$"  (\sp -> XVar sp (UName (NameVar "app"))) InfixRight 1 ]

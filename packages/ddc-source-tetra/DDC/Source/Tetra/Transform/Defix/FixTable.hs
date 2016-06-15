{-# LANGUAGE TypeFamilies #-}
-- | Defines the table that tracks what precedence and associativity
--   infix operators have. The config for common operators is currently
--   hard-coded, rather than being configurable in the source language.
--
module DDC.Source.Tetra.Transform.Defix.FixTable
        ( FixTable      (..)
        , FixDef        (..)
        , InfixAssoc    (..)
        , lookupDefInfixOfSymbol
        , lookupDefPrefixOfSymbol
        , getInfixDefOfSymbol
        , defaultFixTable)
where
import DDC.Source.Tetra.Transform.Defix.Error
import DDC.Source.Tetra.Exp.Source
import Data.List
import qualified Data.Text              as Text


-- | Table of infix operator definitions.
data FixTable l
        = FixTable [FixDef l]


-- | Infix operator definition.
data FixDef l
        -- A prefix operator
        = FixDefPrefix
        { -- String of the operator
          fixDefSymbol  :: String

          -- Expression to rewrite the operator to, 
          -- given the annotation of the original symbol.
        , fixDefExp     :: GXAnnot l -> GExp l }

        -- An infix operator.
        | FixDefInfix
        { -- String of the operator.
          fixDefSymbol  :: String
        
          -- Expression to rewrite the operator to, 
          -- given the annotation of the original symbol.
        , fixDefExp     :: GXAnnot l -> GExp l

          -- Associativity of infix operator.
        , fixDefAssoc   :: InfixAssoc
        
          -- Precedence of infix operator.
        , fixDefPrec    :: Int }


-- | Infix associativity.
data InfixAssoc
        -- | Left associative.
        ---
        --      x * y * z => * (* x y) z
        = InfixLeft

        -- | Right associative.
        ---
        --      x * y * z => * x (* y z)
        | InfixRight

        -- | Non associative.
        ---
        --      x * y * z => error
        | InfixNone
        deriving (Show, Eq)


-- | Lookup the `FixDefInfix` corresponding to a symbol name, if any.
lookupDefInfixOfSymbol  :: FixTable l -> String -> Maybe (FixDef l)
lookupDefInfixOfSymbol (FixTable defs) str
        = find (\def -> case def of
                         FixDefInfix{}  -> fixDefSymbol def == str
                         _              -> False)
                defs


-- | Lookup the `FixDefPrefix` corresponding to a symbol name, if any.
lookupDefPrefixOfSymbol  :: FixTable l -> String -> Maybe (FixDef l)
lookupDefPrefixOfSymbol (FixTable defs) str
        = find (\def -> case def of
                         FixDefPrefix{} -> fixDefSymbol def == str
                         _              -> False)
                defs


-- | Get the precedence of an infix symbol, else Error.
getInfixDefOfSymbol 
        :: GXAnnot l
        -> FixTable l
        -> String 
        -> Either (Error l) (FixDef l)

getInfixDefOfSymbol a table str
 = case lookupDefInfixOfSymbol table str of
        Nothing         -> Left  (ErrorNoInfixDef a str)
        Just def        -> Right def


-- | Default fixity table for infix operators.
defaultFixTable :: GXBoundVar l ~ Bound => FixTable l
defaultFixTable
 = FixTable 
        [ FixDefPrefix  "-"     (xvar "neg")
        , FixDefPrefix  "¬"     (xvar "not")

        -- Operators defined in the Haskell Prelude.
        , FixDefInfix   "∘"     (xvar "compose")        InfixRight 9

        , FixDefInfix   "*"     (xvar "mul")            InfixLeft  7

        , FixDefInfix   "+"     (xvar "add")            InfixLeft  6
        , FixDefInfix   "-"     (xvar "sub")            InfixLeft  6

        , FixDefInfix   "∪"     (xvar "intersect")      InfixLeft  6
        , FixDefInfix   "∩"     (xvar "union")          InfixLeft  6

        , FixDefInfix   "=="    (xvar "eq")             InfixNone  4
        , FixDefInfix   "/="    (xvar "neq")            InfixNone  4
        , FixDefInfix   "<"     (xvar "lt")             InfixNone  4
        , FixDefInfix   "<="    (xvar "le")             InfixNone  4
        , FixDefInfix   ">"     (xvar "gt")             InfixNone  4
        , FixDefInfix   ">="    (xvar "ge")             InfixNone  4

        , FixDefInfix   "/\\"   (xvar "and")            InfixRight 3
        , FixDefInfix   "∧"     (xvar "and")            InfixRight 3

        , FixDefInfix   "\\/"   (xvar "or")             InfixRight 3
        , FixDefInfix   "∨"     (xvar "or")             InfixRight 3

        , FixDefInfix   "$"     (xvar "apply")          InfixRight 1 

        -- String pasting.
        --   These associate to the right so that when text objects are formed by
        --   pasting several together, then spine of the data structure leans to 
        --   the right, as do cons lists.
        , FixDefInfix   "%"  (xvar "paste")             InfixRight 6
        , FixDefInfix   "%%" (xvar "pastes")            InfixRight 6
        ]

 where  xvar str sp 
         = XAnnot sp $ XVar (UName $ Text.pack str)


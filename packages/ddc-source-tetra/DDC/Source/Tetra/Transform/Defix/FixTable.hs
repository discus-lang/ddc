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
import DDC.Source.Tetra.Exp.Generic
import DDC.Source.Tetra.Prim
import Data.List
import qualified DDC.Type.Exp           as T


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
        , fixDefExp     :: GAnnot l -> GExp l }

        -- An infix operator.
        | FixDefInfix
        { -- String of the operator.
          fixDefSymbol  :: String
        
          -- Expression to rewrite the operator to, 
          -- given the annotation of the original symbol.
        , fixDefExp     :: GAnnot l -> GExp l

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
        :: GAnnot l
        -> FixTable l
        -> String 
        -> Either (Error l) (FixDef l)

getInfixDefOfSymbol a table str
 = case lookupDefInfixOfSymbol table str of
        Nothing         -> Left  (ErrorNoInfixDef a str)
        Just def        -> Right def


-- | Default fixity table for infix operators.
defaultFixTable :: GBound l ~ T.Bound Name => FixTable l
defaultFixTable
 = FixTable 
        -- Numeric operators.
        [ FixDefPrefix "-"  (\sp -> XVar sp (T.UName (NameVar "neg")))
        , FixDefInfix  "*"  (\sp -> XVar sp (T.UName (NameVar "mul")))    InfixLeft  7
        , FixDefInfix  "+"  (\sp -> XVar sp (T.UName (NameVar "add")))    InfixLeft  6
        , FixDefInfix  "-"  (\sp -> XVar sp (T.UName (NameVar "sub")))    InfixLeft  6

        -- String pasting.
        --   These associate to the right so that when text objects are formed by
        --   pasting several together, then spine of the data structure leans to 
        --   the right, as do cons lists.
        , FixDefInfix  "%"  (\sp -> XVar sp (T.UName (NameVar "paste")))  InfixRight 6
        , FixDefInfix  "%%" (\sp -> XVar sp (T.UName (NameVar "pastes"))) InfixRight 6

        -- Equality and inequalities.
        , FixDefInfix  "==" (\sp -> XVar sp (T.UName (NameVar "eq" )))    InfixNone  5
        , FixDefInfix  "/=" (\sp -> XVar sp (T.UName (NameVar "neq")))    InfixNone  5
        , FixDefInfix  "<"  (\sp -> XVar sp (T.UName (NameVar "lt" )))    InfixNone  5
        , FixDefInfix  "<=" (\sp -> XVar sp (T.UName (NameVar "le" )))    InfixNone  5
        , FixDefInfix  ">"  (\sp -> XVar sp (T.UName (NameVar "gt" )))    InfixNone  5
        , FixDefInfix  ">=" (\sp -> XVar sp (T.UName (NameVar "ge" )))    InfixNone  5

        -- Function application.
        , FixDefInfix  "$"  (\sp -> XVar sp (T.UName (NameVar "apply")))  InfixRight 1 ]



module Token where
import Text.Lexer.Inchworm.Char


data Located a
        = Located FilePath Location a
        deriving Show

data Token
        = KNewLine
        | KComment      String
        | KPunc         String
        | KKeyWord      String
        | KVar          String
        | KCon          String
        | KOp           String
        | KLit          Lit
        deriving Show


data Lit
        = LInteger      Integer
        | LChar         Char
        | LString       String
        deriving Show


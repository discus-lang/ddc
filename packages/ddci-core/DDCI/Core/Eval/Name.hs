
module DDCI.Core.Eval.Name 
        ( Name    (..)
        , Loc     (..)
        , Rgn     (..)
        , PrimCon (..)
        , PrimOp  (..)
        , readName
        , lexString)
where
import DDC.Base.Pretty
import DDC.Base.Lexer
import DDC.Core.Parser.Lexer
import DDC.Core.Parser.Tokens
import DDC.Type.Transform.Rename
import Data.Char
import Data.Maybe


-- | Names of things.
-- 
--   Names are defined by the client (this interpreter) instead of in the 
--   ddc-core package. This is so we inject the names of primitive constructors
--   and operators that vary from client to client.
--
data Name 
        -- Names whose types are bound in the environment.
        = NameVar     String    -- ^ Variables
        | NameCon     String    -- ^ Constructors
        | NameLoc     Loc       -- ^ Store location
        | NameRgn     Rgn       -- ^ Region handle

        -- Names whose types are baked in, and should be attached to 
        -- the `Bound` constructor that they appear in.
        | NameInt     Integer   -- ^ Integer literals
        | NamePrimCon PrimCon   -- ^ Primitive constructors (eg Unit)
        | NamePrimOp  PrimOp    -- ^ Primitive operators    (eg add, sub)
        deriving (Show, Eq, Ord)
        

instance Pretty Name where
 ppr nn
  = case nn of
        NameVar     v   -> text v
        NameCon     c   -> text c
        NameInt     i   -> text (show i)
        NameLoc     l   -> ppr l
        NameRgn     r   -> ppr r
        NamePrimCon c   -> ppr c
        NamePrimOp  op  -> ppr op


-- Locs and Rgns --------------------------------------------------------------
-- | A store location.
data Loc
        = Loc Int
        deriving (Eq, Ord, Show)

instance Pretty Loc where
 ppr (Loc l)    
        = text "L" <> text (show l) <> text "#"
 

-- | Region handles describe what region a store binding is in.
data Rgn
        = Rgn Int
        deriving (Eq, Ord, Show)

instance Pretty Rgn where
 ppr (Rgn r)    
        = text "R" <> text (show r) <> text "#"


-- PrimCons -------------------------------------------------------------------
-- | A primitive constructor.
data PrimCon
        = PrimTyConUnit         -- ^ Unit type constructor
        | PrimDaConUnit         -- ^ Unit data constructor
        | PrimTyConInt          -- ^ Int  type constructor.
        | PrimTyConString       -- ^ String type constructor.

        -- Implement lists as primitives until we have data decls working
        | PrimTyConList         -- ^ List data type constructor.
        | PrimDaConNil          -- ^ Nil data constructor.
        | PrimDaConCons         -- ^ Cons data constructor.
        deriving (Show, Eq, Ord)


instance Pretty PrimCon where
 ppr con
  = case con of
        PrimTyConUnit   -> text "Unit"
        PrimDaConUnit   -> text "()"
        PrimTyConInt    -> text "Int"
        PrimTyConString -> text "String"
        PrimTyConList   -> text "List"
        PrimDaConNil    -> text "Nil"
        PrimDaConCons   -> text "Cons"


-- PrimOps --------------------------------------------------------------------
-- | A primitive operator.
data PrimOp
        = PrimOpNegInt
        | PrimOpAddInt
        | PrimOpSubInt
        | PrimOpMulInt
        | PrimOpDivInt
        | PrimOpEqInt
        | PrimOpUpdateInt
        deriving (Show, Eq, Ord)


instance Pretty PrimOp where
 ppr op
  = case op of
        PrimOpNegInt    -> text "negInt"
        PrimOpAddInt    -> text "addInt"
        PrimOpSubInt    -> text "subInt"
        PrimOpMulInt    -> text "mulInt"
        PrimOpDivInt    -> text "divInt"

        PrimOpEqInt     -> text "eqInt"

        PrimOpUpdateInt -> text "updateInt"


-- Parsing --------------------------------------------------------------------
-- | Read the name of a primitive operator or constructor.
readName :: String -> Maybe Name
readName []     = Nothing
readName str@(c:rest)
        -- primops
        | isLower c    
        = case (c:rest) of
                "negInt"        -> Just $ NamePrimOp PrimOpNegInt
                "addInt"        -> Just $ NamePrimOp PrimOpAddInt
                "subInt"        -> Just $ NamePrimOp PrimOpSubInt
                "mulInt"        -> Just $ NamePrimOp PrimOpMulInt
                "divInt"        -> Just $ NamePrimOp PrimOpDivInt
                "eqInt"         -> Just $ NamePrimOp PrimOpEqInt
                "updateInt"     -> Just $ NamePrimOp PrimOpUpdateInt
                _               -> Just $ NameVar str
        
        -- region handles
        | c == 'R'
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        = Just $ NameRgn (Rgn $ read ds)
        
        -- store locations
        | c == 'L'
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        = Just $ NameLoc (Loc $ read ds)
        
        -- units
        | str == "Unit" = Just $ NamePrimCon PrimTyConUnit
        | str == "()"   = Just $ NamePrimCon PrimDaConUnit

        -- integers
        | str == "Int"  = Just $ NamePrimCon PrimTyConInt

        | (ds, "")      <- span isDigit str
        = Just $ NameInt (read ds)        
        
        -- implement lists as primitive until we have data type decls implemented
        | str == "List" = Just $ NamePrimCon PrimTyConList
        | str == "Nil"  = Just $ NamePrimCon PrimDaConNil
        | str == "Cons" = Just $ NamePrimCon PrimDaConCons

        -- other constructors
        | isUpper c
        = Just $ NameCon str
        
        | otherwise
        = Nothing

readName_ :: String -> Name
readName_ str
        = fromMaybe (error $ "can't rename token " ++ str)
        $ readName str


-- | Lex a string to tokens, using our own names.
lexString :: Int -> String -> [Token (Tok Name)]
lexString lineStart str
 = map rn $ lexExp lineStart str
 where rn (Token t sp) = Token (rename readName_ t) sp
 

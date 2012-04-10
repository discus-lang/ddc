
module DDC.Core.Eval.Name 
        ( Name    (..)
        , PrimCon (..)
        , PrimOp  (..)
        , Loc     (..)
        , Rgn     (..)
        , Cap     (..)
        , readName
        , lexString)
where
import DDC.Base.Pretty
import DDC.Base.Lexer
import DDC.Core.Parser.Lexer
import DDC.Core.Parser.Tokens
import Data.Char


-- | Names of things recognised by the evaluator.
-- 
data Name 
        -- Names whose types are bound in the environments.
        = NameVar     String     -- ^ User variables.
        | NameCon     String     -- ^ User constructors.

        -- Names whose types are baked in, and should be attached to 
        -- the `Bound` constructor that they appear in.
        | NameInt     Integer    -- ^ Integer literals (which data constructors).
        | NamePrimCon PrimCon    -- ^ Primitive constructors (eg @List, Nil@).
        | NamePrimOp  PrimOp     -- ^ Primitive operators    (eg @addInt, subInt@).

        | NameLoc     Loc        -- ^ Store locations.
        | NameRgn     Rgn        -- ^ Region handles.
        | NameCap     Cap        -- ^ Store capabilities.
        deriving (Show, Eq, Ord)
        

instance Pretty Name where
 ppr nn
  = case nn of
        NameVar     v   -> text v
        NameCon     c   -> text c
        NameInt     i   -> text (show i)
        NamePrimCon c   -> ppr c
        NamePrimOp  op  -> ppr op
        NameLoc     l   -> ppr l
        NameRgn     r   -> ppr r
        NameCap     p   -> ppr p


-- Locations ------------------------------------------------------------------
-- | A store location.
--
--  These are pretty printed like @L4#@.
data Loc
        = Loc Int
        deriving (Eq, Ord, Show)

instance Pretty Loc where
 ppr (Loc l)    
        = text "L" <> text (show l) <> text "#"
 

-- Regions --------------------------------------------------------------------
-- | A region handle.
--
--  These are pretty printed like @R5#@.
data Rgn
        = Rgn Int
        deriving (Eq, Ord, Show)

instance Pretty Rgn where
 ppr (Rgn r)    
        = text "R" <> text (show r) <> text "#"


-- Capabilities --------------------------------------------------------------
-- | These are primitive witnesses that guarantee the associated property
--   of the program. Ostensibly, they are only introduced by the system
--   at runtime, but for testing purposes we can also inject them into
--   the source program.
data Cap
        -- | Witness that a region is global.
        --   Global regions live for the duration of the program and are not
        --   deallocated in a stack like manner. This lets us hide the use of
        --   such regions, and rely on the garbage collector to reclaim the
        --   space.
        = CapGlobal   -- global   :: [r: %]. Global r

        -- | Witness that a region is constant.
        --   This lets us purify read and allocation effects on it,
        --   and prevents it from being Mutable.
        | CapConst    -- const    :: [r: %]. Const r
        
        -- | Witness that a region is mutable.
        --   This lets us update objects in the region, 
        --   and prevents it from being Constant.
        | CapMutable  -- mutable  :: [r: %]. Mutable r

        -- | Witness that a region is lazy.
        --   This lets is allocate thunks into the region,
        --   and prevents it from being Manifest.
        | CapLazy     -- lazy     :: [r: %].Lazy r
        
        -- | Witness that a region is manifest.
        --   This ensures there are no thunks in the region,
        --   which prevents it from being Lazy.
        | CapManifest -- manifest :: [r: %]. Manifest r
        deriving (Eq, Ord, Show)


instance Pretty Cap where
 ppr cp
  = case cp of
        CapGlobal       -> text "Global#"
        CapConst        -> text "Const#"
        CapMutable      -> text "Mutable#"
        CapLazy         -> text "Lazy#"
        CapManifest     -> text "Manifest#"


-- PrimCons -------------------------------------------------------------------
-- | A primitive constructor.
data PrimCon
        -- Type constructors
        = PrimTyConUnit         -- ^ Unit type constructor (@Unit@).
        | PrimTyConInt          -- ^ @Int@  type constructor.
        | PrimTyConPair         -- ^ @Pair@ type constructor.
        | PrimTyConList         -- ^ @List@ type constructor.

        -- Implement lists as primitives until we have data decls working
        | PrimDaConUnit         -- ^ Unit data constructor (@()@).
        | PrimDaConPr           -- ^ @P@ data construct (pairs).
        | PrimDaConNil          -- ^ @Nil@ data constructor.
        | PrimDaConCons         -- ^ @Cons@ data constructor.
        deriving (Show, Eq, Ord)

instance Pretty PrimCon where
 ppr con
  = case con of
        PrimTyConUnit   -> text "Unit"
        PrimTyConInt    -> text "Int"
        PrimTyConPair   -> text "Pair"
        PrimTyConList   -> text "List"

        PrimDaConUnit   -> text "()"
        PrimDaConPr     -> text "Pr"
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
        | PrimOpCopyInt
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
        PrimOpCopyInt	-> text "copyInt"


-- Parsing --------------------------------------------------------------------
-- | Read a primitive name.
readName :: String -> Maybe Name
readName []     = Nothing
readName str@(c:rest)
        -- primops and variables.
        | isLower c    
        = case (c:rest) of
                "negInt"        -> Just $ NamePrimOp PrimOpNegInt
                "addInt"        -> Just $ NamePrimOp PrimOpAddInt
                "subInt"        -> Just $ NamePrimOp PrimOpSubInt
                "mulInt"        -> Just $ NamePrimOp PrimOpMulInt
                "divInt"        -> Just $ NamePrimOp PrimOpDivInt
                "eqInt"         -> Just $ NamePrimOp PrimOpEqInt
                "updateInt"     -> Just $ NamePrimOp PrimOpUpdateInt
                "copyInt"	-> Just $ NamePrimOp PrimOpCopyInt
                _               -> Just $ NameVar str

        -- units
        | str == "Unit"         = Just $ NamePrimCon PrimTyConUnit
        | str == "()"           = Just $ NamePrimCon PrimDaConUnit

        -- integers
        | str == "Int"          = Just $ NamePrimCon PrimTyConInt

        | (ds, "")              <- span isDigit str
        = Just $ NameInt (read ds)        

        -- pairs
        | str == "Pair"         = Just $ NamePrimCon PrimTyConPair
        | str == "Pr"           = Just $ NamePrimCon PrimDaConPr
        
        -- lists 
        | str == "List"         = Just $ NamePrimCon PrimTyConList
        | str == "Nil"          = Just $ NamePrimCon PrimDaConNil
        | str == "Cons"         = Just $ NamePrimCon PrimDaConCons
        
        -- region handles
        | c == 'R'
        , (ds, "#")             <- span isDigit rest
        , not $ null ds
        = Just $ NameRgn (Rgn $ read ds)
        
        -- store locations
        | c == 'L'
        , (ds, "#")             <- span isDigit rest
        , not $ null ds
        = Just $ NameLoc (Loc $ read ds)
        
        -- store capabilities
        | str == "Global#"      = Just $ NameCap CapGlobal
        | str == "Const#"       = Just $ NameCap CapConst
        | str == "Mutable#"     = Just $ NameCap CapMutable
        | str == "Lazy#"        = Just $ NameCap CapLazy
        | str == "Manifest#"    = Just $ NameCap CapManifest

        -- other constructors
        | isUpper c
        = Just $ NameCon str
        
        | otherwise
        = Nothing


-- | Lex a string to tokens, using primitive names.
--
--   The first argument gives the starting source line number.
lexString :: String -> Int -> String -> [Token (Tok Name)]
lexString sourceName lineStart str
 = map rn $ lexExp sourceName lineStart str
 where rn (Token strTok sp) 
        = case renameTok readName strTok of
                Just t' -> Token t' sp
                Nothing -> Token (KJunk "lexical error") sp

 

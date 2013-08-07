
module DDC.Source.Tetra.Desugar.Defix
        ( Error         (..)
        , InfixTable    (..)
        , InfixDef      (..)
        , InfixAssoc    (..)
        , defaultInfixTable
        , Defix         (..))
where
import DDC.Source.Tetra.Module
import DDC.Source.Tetra.Name
import DDC.Source.Tetra.Exp
import Control.Monad
import Data.List
import qualified DDC.Data.SourcePos     as BP


-- | Things that can go wrong when defixing code.
data Error
        = ErrorBlerk
        deriving Show


-- InfixTable -----------------------------------------------------------------
-- | Table of infix operator definitions.
data InfixTable a n
        = InfixTable [InfixDef a n]


-- | Infix operator definition.
data InfixDef a n
        = InfixDef
        { -- | String of the operator.
          infixDefSymbol        :: String
        
          -- | Expression to rewrite the operator to, 
          --   given the annotation of the original symbol.
        , infixDefExp           :: a -> Exp a n

          -- | Associativity of infix operator.
        , infixDefAssoc         :: InfixAssoc
        
          -- | Precedence of infix operator.
        , infixDefPrec          :: Int }


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


-- | Lookup the `InfixDef` corresponding to a symbol name, if any.
lookupInfixDefOfSymbol :: InfixTable a n -> String -> Maybe (InfixDef a n)
lookupInfixDefOfSymbol (InfixTable defs) str
        = find (\def -> infixDefSymbol def == str) defs


-------------------------------------------------------------------------------
-- | Default fixity table for infix operators.
defaultInfixTable :: InfixTable BP.SourcePos Name
defaultInfixTable
 = InfixTable 
        [ InfixDef "*"  (\sp -> XVar sp (UName (NameVar "mul"))) InfixLeft  7
        , InfixDef "+"  (\sp -> XVar sp (UName (NameVar "add"))) InfixLeft  6
        , InfixDef "-"  (\sp -> XVar sp (UName (NameVar "sub"))) InfixLeft  6 
        , InfixDef "==" (\sp -> XVar sp (UName (NameVar "eq" ))) InfixNone  5
        , InfixDef "/=" (\sp -> XVar sp (UName (NameVar "neq"))) InfixNone  5
        , InfixDef "<"  (\sp -> XVar sp (UName (NameVar "lt" ))) InfixNone  5
        , InfixDef "<=" (\sp -> XVar sp (UName (NameVar "le" ))) InfixNone  5
        , InfixDef ">"  (\sp -> XVar sp (UName (NameVar "gt" ))) InfixNone  5
        , InfixDef ">=" (\sp -> XVar sp (UName (NameVar "ge" ))) InfixNone  5
        , InfixDef "$"  (\sp -> XVar sp (UName (NameVar "app"))) InfixRight 1 ]


-- Defix ----------------------------------------------------------------------
class Defix (c :: * -> * -> *) where
 defix  :: InfixTable a n
        -> c a n
        -> Either Error (c a n)


-- Module ---------------------------------------------------------------------
instance Defix Module where
 defix table mm 
  = do  tops'   <- mapM (defix table) (moduleTops mm)
        return  $ mm { moduleTops = tops' }


-- Top ------------------------------------------------------------------------
instance Defix Top where
 defix table tt
  = case tt of
        TopBind a b x   -> liftM (TopBind a b) (defix table x)


-- Exp ------------------------------------------------------------------------
instance Defix Exp where
 defix table xx
  = let down = defix table
    in case xx of
        XVar{}          -> return xx
        XCon{}          -> return xx
        XLAM  a b x     -> liftM  (XLAM  a b) (down x)
        XLam  a b x     -> liftM  (XLam  a b) (down x)
        XApp  a x1 x2   -> liftM2 (XApp  a)   (down x1)  (down x2)
        XLet  a lts x   -> liftM2 (XLet  a)   (down lts) (down x)
        XCase a x alts  -> liftM2 (XCase a)   (down x)   (mapM down alts)
        XCast a c x     -> liftM  (XCast a c) (down x)
        XType{}         -> return xx
        XWitness{}      -> return xx
        XDefix a xs     -> defixExps a table xs
        XOp{}           -> return xx


-- Lets -----------------------------------------------------------------------
instance Defix Lets where
 defix table lts
  = let down = defix table
    in case lts of
        LLet b x        -> liftM (LLet b) (down x)
        LLetRegions{}   -> return lts


-- Alt ------------------------------------------------------------------------
instance Defix Alt where
 defix table aa
  = let down = defix table
    in case aa of
        AAlt p x        -> liftM (AAlt p) (down x)


-- defixExps ------------------------------------------------------------------
-- | Defix the body of a XDefix node.
defixExps 
        :: a -> InfixTable a n 
        -> [Exp a n] 
        -> Either Error (Exp a n)

defixExps a table xs
        -- If the first element is an XOp then we've got a prefix application.
        | XOp aOp str : xsRest    <- xs
        , Just  def             <- lookupInfixDefOfSymbol table str
        , Right x'              <- defixExps a table xsRest
        = Right (XApp a (infixDefExp def aOp) x')

        | otherwise
        = Right (XDefix a xs)



module DDC.Source.Tetra.Desugar.Defix
        ( Error         (..)
        , InfixTable    (..)
        , InfixDef      (..)
        , InfixAssoc    (..)
        , defaultInfixTable
        , Defix         (..))
where
import DDC.Source.Tetra.Compounds
import DDC.Source.Tetra.Module
import DDC.Source.Tetra.Name
import DDC.Source.Tetra.Exp
import DDC.Data.ListUtils
import Control.Monad
import Data.List
import Data.Maybe
import qualified DDC.Data.SourcePos     as BP


-- | Things that can go wrong when defixing code.
data Error
        = ErrorNoInfixDef
        { errorSymbol           :: String }
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
lookupInfixDefOfSymbol  :: InfixTable a n -> String -> Maybe (InfixDef a n)
lookupInfixDefOfSymbol (InfixTable defs) str
        = find (\def -> infixDefSymbol def == str) defs


-- | Get the precedence of an infix symbol, else Error.
getInfixDefOfSymbol :: InfixTable a n -> String -> Either Error (InfixDef a n)
getInfixDefOfSymbol table str
 = case lookupInfixDefOfSymbol table str of
        Nothing         -> Left  (ErrorNoInfixDef str)
        Just def        -> Right def


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

        XDefix a xs     
         -> do  xs'     <- mapM down xs
                defixExps a table xs'

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
        :: a                    -- ^ Annotation from original XDefix node.
        -> InfixTable a n       -- ^ Table of infix defs.
        -> [Exp a n]            -- ^ Body of the XDefix node.
        -> Either Error (Exp a n)

defixExps a table xx
 = case xx of
        -- If there are no elements then we're screwed.
        -- Maybe the parser is wrong or defixInfix has lost them.
        []      ->  error "ddc-source-tetra.defixExps: no expressions"

        -- If there is only one element then we're done.
        [x]     -> Right x

        -- Keep calling defixInfix until we've resolved all the ops.
        x : xs 
         -> case defixInfix a table xx of
                -- Defixer found errors.
                Left  errs      -> Left errs
                
                -- Defixer didn't find any infix ops, so whatever is leftover
                -- is a standard prefix application.
                Right Nothing   -> Right $ xApps a x xs

                -- Defixer made progress, so keep calling it.
                Right (Just xs') -> defixExps a table xs'


-- | Try to defix some expressions.
--   If we make progress then return `Just` the new expressions, 
--   otherwise Nothing.
defixInfix
        :: a                    -- ^ Annotation from original XDefix node.
        -> InfixTable a n       -- ^ Table of infix defs.
        -> [Exp a n]            -- ^ Body of the XDefix node.
        -> Either Error (Maybe [Exp a n])

defixInfix a table xs
        -- If the first element is an XOp then we've got a prefix application.
        -- WRONG: distinguish between op wrapped in brackets and not
        --        doing "(+) a b" is ok but "+ a b" is not.
        --        we still need to use the infixtable to resolve the first (+) though.
        --        We should also be able to pass infix ops to other functions
        --         like "f (+)". Need to resolve vars separately.
        | XOp aOp str : xsRest  <- xs
        , Just  def             <- lookupInfixDefOfSymbol table str
        = Right (Just (infixDefExp def aOp : xsRest))
        
        -- Get the list of infix ops in the expression.
        | opStrs        <- mapMaybe (\x -> case x of
                                            XOp _ str -> Just str
                                            _         -> Nothing)
                                    xs
        = case opStrs of
            []     -> Right Nothing
            _      -> defixInfix_ops a table xs opStrs

defixInfix_ops _a table _xs opStrs
 = do   
        -- Lookup infix info for symbols.
        defs    <- mapM (getInfixDefOfSymbol table) opStrs
        let precs       = map infixDefPrec  defs
        
        -- Get the highest precedence of all symbols.
        let Just precHigh = takeMaximum precs
   
        -- Get the list of all ops having this highest precedence.
        let opsHigh     = [ op   | op    <- opStrs
                                 | prec  <- precs,  prec == precHigh ]

        -- Get the list of associativities for just the ops with
        -- highest precedence.
        defsHigh <- mapM (getInfixDefOfSymbol table) opsHigh
        let assocsHigh  = map infixDefAssoc defsHigh

        case nub assocsHigh of
         [InfixLeft]    -> error "defixInfix: lefty"
         [InfixRight]   -> error "defixInfix: righty"
         [InfixNone]    -> error "defixInfix: none"

         _              -> error "defixInfix: mixed"


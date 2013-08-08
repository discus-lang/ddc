
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
data Error a n
        -- | Infix operator symbol has no infix definition.
        = ErrorNoInfixDef
        { errorSymbol           :: String }

        -- | Two non-associative operators with the same precedence.
        | ErrorDefixNonAssoc
        { errorOp1              :: String
        , errorAnnot1           :: a
        , errorOp2              :: String
        , errorAnnot2           :: a }

        -- | Two operators of different associativies with same precedence.
        | ErrorDefixMixedAssoc 
        { errorAnnot            :: a
        , errorOps              :: [String] }

        -- | Infix expression is malformed.
        --   Eg "+ 3" or "2 + + 2"
        | ErrorMalformed
        { errorExp              :: Exp a n }
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
getInfixDefOfSymbol 
        :: InfixTable a n 
        -> String 
        -> Either (Error a n) (InfixDef a n)

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
        -> Either (Error a n) (c a n)


instance Defix Module where
 defix table mm 
  = do  tops'   <- mapM (defix table) (moduleTops mm)
        return  $ mm { moduleTops = tops' }


instance Defix Top where
 defix table tt
  = case tt of
        TopBind a b x   -> liftM (TopBind a b) (defix table x)


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

        XInfixOp{}      -> return xx
        
        XInfixVar a str
         -> case lookupInfixDefOfSymbol table str of
                Just def -> return (infixDefExp def a)
                Nothing  -> Left $ ErrorNoInfixDef str


instance Defix Lets where
 defix table lts
  = let down = defix table
    in case lts of
        LLet b x        -> liftM (LLet b) (down x)
        LLetRegions{}   -> return lts


instance Defix Alt where
 defix table aa
  = let down = defix table
    in case aa of
        AAlt p x        -> liftM (AAlt p) (down x)


-- defixExps ------------------------------------------------------------------
-- | Defix the body of a XDefix node.
--   TODO: We first need to convert plain applications.
--         This defixer needs  f a + g b  to be pre-processed into (f a) + (g b)
defixExps 
        :: a                    -- ^ Annotation from original XDefix node.
        -> InfixTable a n       -- ^ Table of infix defs.
        -> [Exp a n]            -- ^ Body of the XDefix node.
        -> Either (Error a n) (Exp a n)

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
        -> Either (Error a n) (Maybe [Exp a n])

defixInfix a table xs
        -- Get the list of infix ops in the expression.
        | spOpStrs     <- mapMaybe (\x -> case x of
                                            XInfixOp sp str -> Just (sp, str)
                                            _               -> Nothing)
                                    xs
        = case spOpStrs of
            []     -> Right Nothing
            _      -> defixInfix_ops a table xs spOpStrs

defixInfix_ops sp table xs spOpStrs
 = do   
        let (_opSps, opStrs) = unzip spOpStrs

        -- Lookup infix info for symbols.
        defs    <- mapM (getInfixDefOfSymbol table) opStrs
        let precs       = map infixDefPrec  defs
        
        -- Get the highest precedence of all symbols.
        let Just precHigh = takeMaximum precs
   
        -- Get the list of all ops having this highest precedence.
        let opsHigh     = nub
                        $ [ op   | (op, prec) <- zip opStrs precs
                                 , prec == precHigh ]
                                 
        -- Get the list of associativities for just the ops with
        -- highest precedence.
        defsHigh <- mapM (getInfixDefOfSymbol table) opsHigh
        let assocsHigh  = map infixDefAssoc defsHigh

        case nub assocsHigh of
         [InfixLeft]    
          -> do xs'     <- defixInfixLeft  sp table precHigh xs
                return $ Just xs'

         [InfixRight]   
          -> do xs'     <- defixInfixRight sp table precHigh (reverse xs)
                return $ Just (reverse xs')
         
         [InfixNone]
          -> do xs'     <- defixInfixNone  sp table precHigh xs
                return $ Just (reverse xs')

         _ -> Left $ ErrorDefixMixedAssoc sp opsHigh


-- | Defix some left associative ops.
defixInfixLeft 
        :: a -> InfixTable a n -> Int 
        -> [Exp a n] -> Either (Error a n) [Exp a n]

defixInfixLeft sp table precHigh (x1 : XInfixOp spo op : x2 : xs)
        | Just def      <- lookupInfixDefOfSymbol table op
        , infixDefPrec def == precHigh
        =       Right (XApp sp (XApp sp (infixDefExp def spo) x1) x2 : xs)

        | otherwise
        = do    xs'     <- defixInfixLeft sp table precHigh (x2 : xs)
                Right   $ x1 : XInfixOp spo op : xs'

defixInfixLeft sp _ _ xs
        = Left $ ErrorMalformed (XDefix sp xs)


-- | Defix some right associative ops.
--   The input expression list is reversed, so we can eat the operators left
--   to right. However, be careful to build the App node the right way around.
defixInfixRight
        :: a -> InfixTable a n -> Int 
        -> [Exp a n] -> Either (Error a n) [Exp a n]

defixInfixRight sp table precHigh (x2 : XInfixOp spo op : x1 : xs)
        | Just def      <- lookupInfixDefOfSymbol table op
        , infixDefPrec def == precHigh
        =       Right (XApp sp (XApp sp (infixDefExp def spo) x1) x2 : xs)

        | otherwise
        = do    xs'     <- defixInfixRight sp table precHigh (x1 : xs)
                Right   $ x2 : XInfixOp spo op : xs'

defixInfixRight sp _ _ xs
        = Left $ ErrorMalformed (XDefix sp xs)


-- | Defix non-associative ops.
defixInfixNone 
        :: a -> InfixTable a n -> Int
        -> [Exp a n] -> Either (Error a n) [Exp a n]

defixInfixNone sp table precHigh xx
        -- If there are two ops in a row that are non-associative and have
        -- the same precedence then we don't know which one should come first.
        | _ : XInfixOp sp2 op2 : _ : XInfixOp sp4 op4 : _ <- xx
        , Just def2     <- lookupInfixDefOfSymbol table op2
        , Just def4     <- lookupInfixDefOfSymbol table op4
        , infixDefPrec def2 == infixDefPrec def4
        = Left  $ ErrorDefixNonAssoc op2 sp2 op4 sp4

        -- Found a use of the operator of interest.
        | x1 : XInfixOp sp2 op2 : x3 : xs       <- xx
        , Just def2     <- lookupInfixDefOfSymbol table op2
        , infixDefPrec def2 == precHigh
        = Right $ (XApp sp (XApp sp (infixDefExp def2 sp2) x1) x3) : xs

        -- Some other operator.
        | x1 : x2@(XInfixOp{}) : x3 : xs       <- xx
        = case defixInfixNone sp table precHigh (x3 : xs) of
                Right xs'       -> Right (x1 : x2 : xs')
                Left errs       -> Left errs

        | otherwise
        = Left $ ErrorMalformed (XDefix sp xx)


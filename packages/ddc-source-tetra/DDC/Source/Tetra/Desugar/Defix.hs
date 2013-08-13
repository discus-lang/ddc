
module DDC.Source.Tetra.Desugar.Defix
        ( FixTable      (..)
        , FixDef        (..)
        , InfixAssoc    (..)
        , defaultFixTable
        , Error         (..)
        , Defix         (..))
where
import DDC.Source.Tetra.Desugar.Defix.FixTable
import DDC.Source.Tetra.Desugar.Defix.Error
import DDC.Source.Tetra.Compounds
import DDC.Source.Tetra.Module
import DDC.Source.Tetra.Exp
import DDC.Data.ListUtils
import Control.Monad
import Data.List
import Data.Maybe


-- Defix ----------------------------------------------------------------------
class Defix (c :: * -> * -> *) where
 defix  :: FixTable a n
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
                xs_apps <- defixApps a table xs'
                defixExps a table xs_apps

        XInfixOp{}      -> return xx
        
        XInfixVar a str
         -> case lookupDefInfixOfSymbol table str of
                Just def -> return (fixDefExp def a)
                Nothing  -> Left $ ErrorNoInfixDef a str


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


-------------------------------------------------------------------------------
-- | Preprocess the body of an XDefix node to insert applications.
--    
--   Takes         f a  +  g b  with five  nodes in the XDefix list.
--   and produces (f a) + (g b) with three nodes in the XDefix list.
--
defixApps 
        :: a
        -> FixTable a n
        -> [Exp a n]
        -> Either (Error a n) [Exp a n]

defixApps a table xx
 = start xx
 where
        -- No expressions, we're done.
        start [] 
         = return []

        -- Single element, we're done.
        start [x]
         = return [x]

        -- Starting operator must be prefix.
        start (XInfixOp aop op : xs)
         | Just def     <- lookupDefPrefixOfSymbol table op
         = munch (fixDefExp def aop) xs

         | otherwise
         = Left $ ErrorMalformed a (XDefix a xx)

        -- Trailing infix operator is malformed.
        start (_ : XInfixOp{} : [])
         = Left $ ErrorMalformed a (XDefix a xx)

        -- Start accumulating an application node.
        start (x1 : xs) 
         = munch x1 xs


        -- Munching is done.
        munch acc []
         = return [acc]

        -- We've hit an infix op, drop the accumulated expression.
        munch acc (xop@XInfixOp{} : xs)
         = do   xs'     <- start xs
                return $ acc : xop : xs'

        -- Add another argument to the application.
        munch acc (x1 : xs)
         = munch (XApp a acc x1) xs


-------------------------------------------------------------------------------
-- | Defix the body of a XDefix node.
--
--   The input needs to have already been preprocessed by defixApps above.
--
defixExps 
        :: a                    -- ^ Annotation from original XDefix node.
        -> FixTable a n         -- ^ Table of infix defs.
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


-- | Try to defix a sequence of expressions and XInfixOp nodes.
defixInfix
        :: a                    -- ^ Annotation from original XDefix node.
        -> FixTable a n         -- ^ Table of infix defs.
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
        defs    <- mapM (getInfixDefOfSymbol sp table) opStrs
        let precs       = map fixDefPrec  defs
        
        -- Get the highest precedence of all symbols.
        let Just precHigh = takeMaximum precs
   
        -- Get the list of all ops having this highest precedence.
        let opsHigh     = nub
                        $ [ op   | (op, prec) <- zip opStrs precs
                                 , prec == precHigh ]
                                 
        -- Get the list of associativities for just the ops with
        -- highest precedence.
        defsHigh <- mapM (getInfixDefOfSymbol sp table) opsHigh
        let assocsHigh  = map fixDefAssoc defsHigh

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
        :: a -> FixTable a n -> Int 
        -> [Exp a n] -> Either (Error a n) [Exp a n]

defixInfixLeft sp table precHigh (x1 : XInfixOp spo op : x2 : xs)
        | Just def      <- lookupDefInfixOfSymbol table op
        , fixDefPrec def == precHigh
        =       Right (XApp sp (XApp sp (fixDefExp def spo) x1) x2 : xs)

        | otherwise
        = do    xs'     <- defixInfixLeft sp table precHigh (x2 : xs)
                Right   $ x1 : XInfixOp spo op : xs'

defixInfixLeft sp _ _ xs
        = Left $ ErrorMalformed sp (XDefix sp xs)


-- | Defix some right associative ops.
--   The input expression list is reversed, so we can eat the operators left
--   to right. However, be careful to build the App node the right way around.
defixInfixRight
        :: a -> FixTable a n -> Int 
        -> [Exp a n] -> Either (Error a n) [Exp a n]

defixInfixRight sp table precHigh (x2 : XInfixOp spo op : x1 : xs)
        | Just def      <- lookupDefInfixOfSymbol table op
        , fixDefPrec def == precHigh
        =       Right (XApp sp (XApp sp (fixDefExp def spo) x1) x2 : xs)

        | otherwise
        = do    xs'     <- defixInfixRight sp table precHigh (x1 : xs)
                Right   $ x2 : XInfixOp spo op : xs'

defixInfixRight sp _ _ xs
        = Left $ ErrorMalformed sp (XDefix sp xs)


-- | Defix non-associative ops.
defixInfixNone 
        :: a -> FixTable a n -> Int
        -> [Exp a n] -> Either (Error a n) [Exp a n]

defixInfixNone sp table precHigh xx
        -- If there are two ops in a row that are non-associative and have
        -- the same precedence then we don't know which one should come first.
        | _ : XInfixOp sp2 op2 : _ : XInfixOp sp4 op4 : _ <- xx
        , Just def2     <- lookupDefInfixOfSymbol table op2
        , Just def4     <- lookupDefInfixOfSymbol table op4
        , fixDefPrec def2 == fixDefPrec def4
        = Left  $ ErrorDefixNonAssoc op2 sp2 op4 sp4

        -- Found a use of the operator of interest.
        | x1 : XInfixOp sp2 op2 : x3 : xs       <- xx
        , Just def2     <- lookupDefInfixOfSymbol table op2
        , fixDefPrec def2 == precHigh
        = Right $ (XApp sp (XApp sp (fixDefExp def2 sp2) x1) x3) : xs

        -- Some other operator.
        | x1 : x2@(XInfixOp{}) : x3 : xs       <- xx
        = case defixInfixNone sp table precHigh (x3 : xs) of
                Right xs'       -> Right (x1 : x2 : xs')
                Left errs       -> Left errs

        | otherwise
        = Left $ ErrorMalformed sp (XDefix sp xx)



module DDC.Core.Parser
        ( module DDC.Base.Parser
        , Parser
        , pExp
        , pWitness)
        
where
import DDC.Core.Exp
import DDC.Core.Parser.Tokens
import DDC.Base.Parser                  ((<?>))
import DDC.Type.Parser                  (pTok)
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Compounds     as T
import qualified DDC.Type.Parser        as T
import Control.Monad.Error


-- | Parser of core language tokens.
type Parser n a
        = P.Parser (Tok n) a


-- Expressions ----------------------------------------------------------------
pExp    :: Ord n => Parser n (Exp () n)
pExp 
 = P.choice
        -- Lambda abstractions
 [ do   pTok KBackSlash
        pTok KRoundBra
        bs      <- P.many1 T.pBinder
        pTok KColon
        t       <- T.pType
        pTok KRoundKet
        pTok KDot

        xBody   <- pExp

        return  $ foldr (XLam ()) xBody
                $ map (\b -> T.makeBindFromBinder b t) bs


        -- let expression
 , do   pTok KLet
        b       <- pBind (T.tBot T.kData)
        pTok KEquals
        x1      <- pExp
        pTok KIn
        x2      <- pExp
        return  $ XLet () (LLet b x1) x2
        

        -- letrec expression
 , do   pTok KLetRec
        pTok KBraceBra
        lets    <- P.sepEndBy1 pLetRecBind (pTok KSemiColon)
        pTok KBraceKet
        pTok KIn
        x       <- pExp
        return  $ XLet () (LRec lets) x


        -- Local region binding
        --   letregion BINDER with { BINDER : TYPE ... } in EXP
        --   letregion BINDER in EXP
 , do   pTok KLetRegion
        br      <- T.pBinder
        let b   = T.makeBindFromBinder br T.kRegion

        P.choice 
         [ do   pTok KWith
                pTok KBraceBra
                wits    <- P.sepBy1 
                           (do  w       <- pVar
                                pTok KColon
                                t       <- T.pTypeApp
                                return  (BName w t))
                           (pTok KSemiColon)
                pTok KBraceKet
                pTok KIn
                x       <- pExp 
                return  $ XLet () (LLetRegion b wits) x 

         , do   pTok KIn
                x       <- pExp
                return $ XLet ()  (LLetRegion b []) x ]


        -- withregion CON in EXP
  , do  pTok KWithRegion
        n       <- pVar
        pTok KIn
        x       <- pExp
        let u   = UName n (T.tBot T.kRegion)
        return  $ XLet () (LWithRegion u) x


        -- case EXP of { ALTS }
  , do  pTok KCase
        x       <- pExp
        pTok KOf 
        pTok KBraceBra
        alts    <- P.sepEndBy1 pAlt (pTok KSemiColon)
        pTok KBraceKet
        return  $ XCase () x alts


        -- purify <WITNESS> EXP
  , do  pTok KPurify
        pTok KAngleBra
        w       <- pWitness
        pTok KAngleKet
        x       <- pExp
        return  $ XCast () (CastPurify w) x


        -- forget <WITNESS> EXP
  , do  pTok KForget
        pTok KAngleBra
        w       <- pWitness
        pTok KAngleKet
        x       <- pExp
        return  $ XCast () (CastForget w) x

        -- APP
 , do   pExpApp
 ]

 <?> "an expression"


-- Applications.
pExpApp :: Ord n => Parser n (Exp () n)
pExpApp 
  = do  x1      <- pExp0
        
        P.choice
         [ do   xs  <- liftM concat $ P.many1 pArgs
                return  $ foldl (XApp ()) x1 xs

         ,      return x1]

 <?> "an expression or application"


-- Comp, Witness or Spec arguments.
pArgs   :: Ord n => Parser n [Exp () n]
pArgs 
 = P.choice
        -- [TYPE]
 [ do   pTok KSquareBra
        t       <- T.pType 
        pTok KSquareKet
        return  [XType t]

        -- [: TYPE0 TYPE0 ... :]
 , do   pTok KSquareColonBra
        ts      <- P.many1 T.pTypeAtom
        pTok KSquareColonKet
        return  $ map XType ts
        
        -- <WITNESS>
 , do   pTok KAngleBra
        w       <- pWitness
        pTok KAngleKet
        return  [XWitness w]
                
        -- <: WITNESS0 WITNESS0 ... :>
 , do   pTok KAngleColonBra
        ws      <- P.many1 pWitnessAtom
        pTok KAngleColonKet
        return  $ map XWitness ws
                
        -- EXP0
 , do   x       <- pExp0
        return  [x]
 ]
 <?> "a type, witness or expression argument"


-- Atomics
pExp0   :: Ord n => Parser n (Exp () n)
pExp0 
 = P.choice
        -- (EXP2)
 [ do   pTok KRoundBra
        t       <- pExp
        pTok KRoundKet
        return  $ t
        
        -- Named type constructors
 , do   con     <- pCon
        return  $ XCon () (UName con (T.tBot T.kData)) 

        -- Literals
 , do   lit     <- pLit
        return  $ XCon () (UName lit (T.tBot T.kData))

        -- Debruijn indices
 , do   i       <- T.pIndex
        return  $ XVar () (UIx   i   (T.tBot T.kData))

        -- Variables
 , do   var     <- pVar
        return  $ XVar () (UName var (T.tBot T.kData)) 
 ]

 <?> "a variable, constructor, or parenthesised type"


-- Case alternatives.
pAlt    :: Ord n => Parser n (Alt () n)
pAlt
 = do   p       <- pPat
        pTok KArrowDash
        x       <- pExp
        return  $ AAlt p x


-- Patterns.
pPat    :: Ord n => Parser n (Pat n)
pPat
 = P.choice
 [ -- _
   do   pTok KUnderscore
        return  $ PDefault

   -- LIT
 , do   nLit    <- pLit
        return  $ PData (UName nLit (T.tBot T.kData)) []

   -- CON BIND BIND ...
 , do   nCon    <- pCon 
        bs      <- P.many pBindPat
        return  $ PData (UName nCon (T.tBot T.kData)) bs]



-- Bind with optional type.
pBind :: Ord n => Type n -> Parser n (Bind n)
pBind tDefault
 = do   b       <- T.pBinder
        P.choice 
         [ do   pTok KColon
                t       <- T.pType
                return  $ T.makeBindFromBinder b t

         , do   return  $ T.makeBindFromBinder b tDefault]


-- Binds in patterns can have no type annotation,
-- or can have an annotation if the whole thing is in parens.
pBindPat :: Ord n => Parser n (Bind n)
pBindPat 
 = P.choice
        -- Plain binder.
 [ do   b       <- T.pBinder
        return  $ T.makeBindFromBinder b (T.tBot T.kData)

        -- Binder with type, wrapped in parens.
 , do   pTok KRoundBra
        b       <- T.pBinder
        pTok KColon
        t       <- T.pType
        pTok KRoundKet
        return  $ T.makeBindFromBinder b t
 ]



-- Bindings -------------------------------------------------------------------
pLetRecBind :: Ord n => Parser n (Bind n, Exp () n)
pLetRecBind 
 = do   b       <- T.pBinder

        ps      <- liftM concat 
                $  P.many pBindParamSpec 
        
        pTok KColon
        tBody   <- T.pType
        let t   = funTypeOfParams ps tBody

        pTok KEquals
        xBody   <- pExp
        let x   = expOfParams () ps xBody

        return  (T.makeBindFromBinder b t, x)


-- | Parse a
pBindParamSpec :: Ord n => Parser n [ParamSpec n]
pBindParamSpec
 = P.choice
        -- Type parameter
 [ do   pTok KSquareBra
        bs      <- P.many1 T.pBinder
        pTok KColon
        t       <- T.pType
        pTok KSquareKet
        return  $ [ ParamType b
                        | b     <- zipWith T.makeBindFromBinder bs (repeat t)]

 , do   pTok KRoundBra
        b       <- T.pBinder
        pTok KColon
        t       <- T.pType
        pTok KRoundKet

        (eff, clo) 
         <- P.choice
                [ do    pTok KBraceBra
                        eff'    <- T.pType
                        pTok KBar
                        clo'    <- T.pType
                        pTok KBraceKet
                        return  (eff', clo')
                
                , do    return  (T.tBot T.kEffect, T.tBot T.kClosure) ]
                

        return  $ [ParamValue (T.makeBindFromBinder b t) eff clo]
 ]


-- | Specification of a function parameter.
--   We can determine the contribution to the type of the function, 
--   as well as its expression based on the parameter.
data ParamSpec n
        = ParamType  (Bind n)
        | ParamValue (Bind n) (Type n) (Type n)


-- | Build the type of a function from specifications of its parameters,
--   and the type of the body.
funTypeOfParams 
        :: [ParamSpec n]        -- ^ Spec of parameters.
        -> Type n               -- ^ Type of body.
        -> Type n               -- ^ Type of whole function.

funTypeOfParams [] tBody        = tBody
funTypeOfParams (p:ps) tBody
 = case p of
        ParamType  b    
         -> TForall b 
                $ funTypeOfParams ps tBody

        ParamValue b eff clo
         -> T.tFun (T.typeOfBind b) eff clo 
                $ funTypeOfParams ps tBody


-- | Build the expression of a function from specifications of its parameters,
--   and the expression for the body.
expOfParams 
        :: a
        -> [ParamSpec n]        -- ^ Spec of parameters.
        -> Exp a n              -- ^ Body of function.
        -> Exp a n              -- ^ Expression of whole function.

expOfParams _ [] xBody            = xBody
expOfParams a (p:ps) xBody
 = case p of
        ParamType b     
         -> XLam a b $ expOfParams a ps xBody
        
        ParamValue b _ _
         -> XLam a b $ expOfParams a ps xBody



-- Witnesses ------------------------------------------------------------------
-- | Top level parser for witnesses.
pWitness :: Ord n  => Parser n (Witness n)
pWitness = pWitnessJoin


-- Witness Joining
pWitnessJoin :: Ord n => Parser n (Witness n)
pWitnessJoin 
   -- WITNESS  or  WITNESS & WITNESS
 = do   w1      <- pWitnessApp
        P.choice 
         [ do   pTok KAmpersand
                w2      <- pWitnessJoin
                return  (WJoin w1 w2)

         , do   return w1 ]


-- Applications
pWitnessApp :: Ord n => Parser n (Witness n)
pWitnessApp 
  = do  (x:xs)  <- P.many1 pWitnessArg
        return  $ foldl WApp x xs

 <?> "a witness expression or application"


-- Function argument
pWitnessArg :: Ord n => Parser n (Witness n)
pWitnessArg 
 = P.choice
 [ -- [TYPE]
   do   pTok KSquareBra
        t       <- T.pType
        pTok KSquareKet
        return  $ WType t

   -- WITNESS
 , do   pWitnessAtom ]


-- Atomics
pWitnessAtom :: Ord n => Parser n (Witness n)
pWitnessAtom 
 = P.choice
   -- (WITNESS)
 [ do    pTok KRoundBra
         w       <- pWitness
         pTok KRoundKet
         return  $ w

   -- Named witness constructors.
 , do    wc     <- pWiCon
         return $ WCon wc 
                
   -- Debruijn indices
 , do    i       <- T.pIndex
         return  $ WVar (UIx   i   (T.tBot T.kWitness))

   -- Variables
 , do    var     <- pVar
         return  $ WVar (UName var (T.tBot T.kWitness)) ]

 <?> "a witness"


-------------------------------------------------------------------------------
-- | Parse a builtin named `WiCon`
pWiCon :: Parser n WiCon
pWiCon  = P.pTokMaybe f
 where  f (KA (KWiConBuiltin wc)) = Just wc
        f _                       = Nothing


-- | Parse a variable name
pVar :: Parser n n
pVar    = P.pTokMaybe f
 where  f (KN (KVar n)) = Just n
        f _             = Nothing


-- | Parse a constructor name
pCon :: Parser n n
pCon    = P.pTokMaybe f
 where  f (KN (KCon n)) = Just n
        f _             = Nothing


-- | Parse a literal
pLit :: Parser n n
pLit    = P.pTokMaybe f
 where  f (KN (KLit n)) = Just n
        f _             = Nothing

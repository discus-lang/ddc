
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
pExp = pExp2

pExp2   :: Ord n => Parser n (Exp () n)
pExp2   
 = P.choice
        -- Lambda abstractions
 [ do   pTok KBackSlash
        pTok KRoundBra
        bs      <- P.many1 T.pBinder
        pTok KColon
        t       <- T.pType
        pTok KRoundKet
        pTok KDot

        xBody   <- pExp2

        return  $ foldr (XLam ()) xBody
                $ map (\b -> T.makeBindFromBinder b t) bs


        -- Let expressions
 , do   pTok KLet
        b       <- T.pBinder
        pTok KColon
        t       <- T.pType
        pTok KEquals
        x1      <- pExp
        pTok KIn
        x2      <- pExp
        return  $ XLet () (LLet (T.makeBindFromBinder b t) x1) x2
        

        -- Local region binding
        --   let region r1 with { w1 : T1 ... } in T2
        --   let region r1 in T2
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
                x       <- pExp2 
                return  $ XLet () (LLetRegion b wits) x 

         , do   pTok KIn
                x       <- pExp2
                return $ XLet ()  (LLetRegion b []) x ]

 , do   pExp1 
 ]

 <?> "an expression"


-- Applications
pExp1   :: Ord n => Parser n (Exp () n)
pExp1 
  = do  (x:xs)  <- liftM concat $ P.many1 pArgs
        return  $ foldl (XApp ()) x xs

 <?> "an expression or application"


-- Comp, Witness or Spec arguments.
pArgs   :: Ord n => Parser n [Exp () n]
pArgs 
 = P.choice
        -- {TYPE}
 [ do   pTok KBraceBra
        t       <- T.pType 
        pTok KBraceKet
        return  [XType t]

        -- {: TYPE0 TYPE0 ... :}
 , do   pTok KBraceColonBra
        ts      <- P.many1 T.pTypeAtom
        pTok KBraceColonKet
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
 <?> "a type type, witness or expression argument"


-- Atomics
pExp0   :: Ord n => Parser n (Exp () n)
pExp0 
 = P.choice
        -- (EXP2)
 [ do   pTok KRoundBra
        t       <- pExp2
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


-- Witnesses ------------------------------------------------------------------
-- | Top level parser for witnesses.
pWitness :: Ord n  => Parser n (Witness n)
pWitness = pWitnessApp


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
 [ -- {TYPE}
   do   pTok KBraceBra
        t       <- T.pType
        pTok KBraceKet
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

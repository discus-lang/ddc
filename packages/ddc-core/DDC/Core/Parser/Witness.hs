
module DDC.Core.Parser.Witness
        ( pWitness
        , pWitnessAtom) 
where
import DDC.Core.Parser.Type
import DDC.Core.Parser.Base
import DDC.Core.Lexer.Tokens
import DDC.Core.Exp
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Compounds     as T
 

-- | Parse a witness expression.
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
        t       <- pType
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

   -- Named constructors
 , do   con     <- pCon
        return  $ WCon (WiConBound $ UName con (T.tBot T.kWitness)) 

   -- Baked-in witness constructors.
 , do    wb     <- pWbCon
         return $ WCon (WiConBuiltin wb)

                
   -- Debruijn indices
 , do    i       <- pIndex
         return  $ WVar (UIx   i   (T.tBot T.kWitness))

   -- Variables
 , do    var     <- pVar
         return  $ WVar (UName var (T.tBot T.kWitness)) ]

 <?> "a witness"

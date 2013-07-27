
module DDC.Core.Parser.Witness
        ( pWitness
        , pWitnessApp
        , pWitnessAtom) 
where
import DDC.Core.Parser.Type
import DDC.Core.Parser.Context
import DDC.Core.Parser.Base
import DDC.Core.Lexer.Tokens
import DDC.Core.Exp
import DDC.Base.Parser                  ((<?>), SourcePos)
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Compounds     as T
import Control.Monad 


-- | Parse a witness expression.
pWitness 
        :: Ord n  
        => Context -> Parser n (Witness SourcePos n)
pWitness c = pWitnessJoin c


-- | Parse a witness join.
pWitnessJoin 
        :: Ord n 
        => Context -> Parser n (Witness SourcePos n)
pWitnessJoin c
   -- WITNESS  or  WITNESS & WITNESS
 = do   w1      <- pWitnessApp c
        P.choice 
         [ do   sp      <- pTokSP (KOp "&")
                w2      <- pWitnessJoin c
                return  (WJoin sp w1 w2)

         , do   return w1 ]


-- | Parse a witness application.
pWitnessApp 
        :: Ord n 
        => Context -> Parser n (Witness SourcePos n)

pWitnessApp c
  = do  (x:xs)  <- P.many1 (pWitnessArgSP c)
        let x'  = fst x
        let sp  = snd x
        let xs' = map fst xs
        return  $ foldl (WApp sp) x' xs'

 <?> "a witness expression or application"


-- | Parse a witness argument.
pWitnessArgSP 
        :: Ord n 
        => Context -> Parser n (Witness SourcePos n, SourcePos)

pWitnessArgSP c
 = P.choice
 [ -- [TYPE]
   do   sp      <- pTokSP KSquareBra
        t       <- pType c
        pTok KSquareKet
        return  (WType sp t, sp)

   -- WITNESS
 , do   pWitnessAtomSP c ]



-- | Parse a variable, constructor or parenthesised witness.
pWitnessAtom   
        :: Ord n 
        => Context -> Parser n (Witness SourcePos n)

pWitnessAtom c   
        = liftM fst (pWitnessAtomSP c)


-- | Parse a variable, constructor or parenthesised witness,
--   also returning source position.
pWitnessAtomSP 
        :: Ord n 
        => Context -> Parser n (Witness SourcePos n, SourcePos)

pWitnessAtomSP c
 = P.choice
   -- (WITNESS)
 [ do   sp      <- pTokSP KRoundBra
        w       <- pWitness c
        pTok KRoundKet
        return  (w, sp)

   -- Named constructors
 , do   (con, sp) <- pConSP
        return  (WCon sp (WiConBound (UName con) (T.tBot T.kWitness)), sp)

   -- Baked-in witness constructors.
 , do   (wb, sp) <- pWbConSP
        return  (WCon sp (WiConBuiltin wb), sp)

                
   -- Debruijn indices
 , do   (i, sp) <- pIndexSP
        return  (WVar sp (UIx   i), sp)

   -- Variables
 , do   (var, sp) <- pVarSP
        return  (WVar sp (UName var), sp) ]

 <?> "a witness"

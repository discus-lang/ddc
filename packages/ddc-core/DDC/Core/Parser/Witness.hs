
module DDC.Core.Parser.Witness
        ( pWitness
        , pWitnessApp
        , pWitnessAtom) 
where
import DDC.Core.Parser.Type
import DDC.Core.Parser.Base
import DDC.Core.Lexer.Tokens
import DDC.Core.Exp
import DDC.Base.Parser                  ((<?>), SourcePos)
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Compounds     as T
import Control.Monad 

-- | Parse a witness expression.
pWitness :: Ord n  => Parser n (Witness SourcePos n)
pWitness = pWitnessJoin


-- | Parse a witness join.
pWitnessJoin :: Ord n => Parser n (Witness SourcePos n)
pWitnessJoin 
   -- WITNESS  or  WITNESS & WITNESS
 = do   w1      <- pWitnessApp
        P.choice 
         [ do   sp      <- pTokSP KAmpersand
                w2      <- pWitnessJoin
                return  (WJoin sp w1 w2)

         , do   return w1 ]


-- | Parse a witness application.
pWitnessApp :: Ord n => Parser n (Witness SourcePos n)
pWitnessApp 
  = do  (x:xs)  <- P.many1 pWitnessArgSP
        let x'  = fst x
        let sp  = snd x
        let xs' = map fst xs
        return  $ foldl (WApp sp) x' xs'

 <?> "a witness expression or application"


-- | Parse a witness argument.
pWitnessArgSP :: Ord n => Parser n (Witness SourcePos n, SourcePos)
pWitnessArgSP 
 = P.choice
 [ -- [TYPE]
   do   sp      <- pTokSP KSquareBra
        t       <- pType
        pTok KSquareKet
        return  (WType sp t, sp)

   -- WITNESS
 , do   pWitnessAtomSP ]



-- | Parse a variable, constructor or parenthesised witness.
pWitnessAtom   :: Ord n => Parser n (Witness SourcePos n)
pWitnessAtom    = liftM fst pWitnessAtomSP


-- | Parse a variable, constructor or parenthesised witness,
--   also returning source position.
pWitnessAtomSP :: Ord n => Parser n (Witness SourcePos n, SourcePos)
pWitnessAtomSP 
 = P.choice
   -- (WITNESS)
 [ do   sp      <- pTokSP KRoundBra
        w       <- pWitness
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

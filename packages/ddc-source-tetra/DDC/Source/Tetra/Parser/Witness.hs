
module DDC.Source.Tetra.Parser.Witness
        ( pWitness
        , pWitnessApp
        , pWitnessAtom) 
where
import DDC.Source.Tetra.Prim
import DDC.Source.Tetra.Exp.Annot
import DDC.Core.Parser
        ( Parser
        , Context (..)
        , pType
        , pConSP
        , pIndexSP
        , pVarSP
        , pTok
        , pTokSP)

import DDC.Core.Lexer.Tokens
import DDC.Base.Parser                  ((<?>), SourcePos)
import qualified DDC.Base.Parser        as P
import qualified DDC.Type.Exp.Simple    as T
import Control.Monad.Except

type SP = SourcePos


-- | Parse a witness expression.
pWitness      :: Context Name -> Parser Name (Witness SP)
pWitness c = pWitnessJoin c


-- | Parse a witness join.
pWitnessJoin  :: Context Name -> Parser Name (Witness SP)
pWitnessJoin c
   -- WITNESS  or  WITNESS & WITNESS
 = do   w1      <- pWitnessApp c
        P.choice 
         [ do   return w1 ]


-- | Parse a witness application.
pWitnessApp   :: Context Name -> Parser Name (Witness SP)
pWitnessApp c
  = do  (x:xs)  <- P.many1 (pWitnessArgSP c)
        let x'  = fst x
        let sp  = snd x
        let xs' = map fst xs
        return  $ foldl (WApp sp) x' xs'

 <?> "a witness expression or application"


-- | Parse a witness argument.
pWitnessArgSP :: Context Name -> Parser Name (Witness SP, SP)
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
pWitnessAtom   :: Context Name -> Parser Name (Witness SP)
pWitnessAtom c   
        = liftM fst (pWitnessAtomSP c)


-- | Parse a variable, constructor or parenthesised witness,
--   also returning source position.
pWitnessAtomSP :: Context Name -> Parser Name (Witness SP, SP)
pWitnessAtomSP c
 = P.choice
   -- (WITNESS)
 [ do   sp      <- pTokSP KRoundBra
        w       <- pWitness c
        pTok KRoundKet
        return  (w, sp)

   -- Named constructors
 , do   (con, sp) <- pConSP
        return  (WCon sp (WiConBound (T.UName con) (T.tBot T.kWitness)), sp)
                
   -- Debruijn indices
 , do   (i, sp) <- pIndexSP
        return  (WVar sp (T.UIx   i), sp)

   -- Variables
 , do   (var, sp) <- pVarSP
        return  (WVar sp (T.UName var), sp) ]

 <?> "a witness"


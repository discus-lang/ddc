
module DDC.Source.Tetra.Parser.Witness
        ( pWitness
        , pWitnessApp
        , pWitnessAtom)
where
import DDC.Source.Tetra.Parser.Type
import DDC.Source.Tetra.Parser.Base
import DDC.Source.Tetra.Exp.Source
import Control.Monad.Except
import DDC.Core.Lexer.Tokens            as K
import qualified DDC.Control.Parser     as P


-- | Parse a witness expression.
pWitness :: Parser Witness
pWitness = pWitnessJoin


-- | Parse a witness join.
pWitnessJoin :: Parser Witness
pWitnessJoin
   -- WITNESS  or  WITNESS & WITNESS
 = do   w1      <- pWitnessApp
        P.choice
         [ do   return w1 ]


-- | Parse a witness application.
pWitnessApp :: Parser Witness
pWitnessApp
  = do  (x:xs)  <- P.many1 pWitnessArgSP
        let x'  = fst x
        let sp  = snd x
        let xs' = map fst xs
        return  $ foldl (\w1 w2 -> WAnnot sp $ WApp w1 w2) x' xs'

 <?> "a witness expression or application"


-- | Parse a witness argument.
pWitnessArgSP :: Parser (Witness, SP)
pWitnessArgSP
 = P.choice
 [ -- [TYPE]
   do   sp      <- pSym SSquareBra
        t       <- pType
        pSym SSquareKet
        return  (WAnnot sp $ WType t, sp)

   -- WITNESS
 , do   pWitnessAtomSP ]



-- | Parse a variable, constructor or parenthesised witness.
pWitnessAtom :: Parser Witness
pWitnessAtom =  liftM fst pWitnessAtomSP


-- | Parse a variable, constructor or parenthesised witness,
--   also returning source position.
pWitnessAtomSP :: Parser (Witness, SP)
pWitnessAtomSP
 = P.choice
   -- (WITNESS)
 [ do   sp      <- pSym SRoundBra
        w       <- pWitness
        pSym SRoundKet
        return  (w, sp)

   -- Named constructors
 , do   (DaConBoundName n, sp) <- pDaConBoundNameSP
        return  ( WAnnot sp $ WCon (WiConBound (UName n) (TBot KData))
                , sp)

   -- Debruijn indices
 , do   (u, sp) <- pBoundIxSP
        return  ( WAnnot sp $ WVar u, sp)

   -- Variables
 , do   (u, sp) <- pBoundNameSP
        return  ( WAnnot sp $ WVar u, sp)
 ]

 <?> "a witness"


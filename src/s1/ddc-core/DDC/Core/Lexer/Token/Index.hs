
module DDC.Core.Lexer.Token.Index
        (scanIndex)
where
import Text.Lexer.Inchworm.Char         as I
import qualified Data.Char              as Char


-- | Scan a deBruijn index.
scanIndex   :: Scanner IO Location [Char] (I.Location, Int)
scanIndex
 = I.munchPred Nothing matchIndex acceptIndex
 where
        matchIndex 0 '^'        = True
        matchIndex 0 _          = False
        matchIndex _ c          = Char.isDigit c

        acceptIndex ('^': xs)
         | not $ null xs        = return (read xs)
        acceptIndex _           = Nothing


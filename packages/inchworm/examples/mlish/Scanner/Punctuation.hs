
module Scanner.Punctuation
        (scanPunctuation)
where
import Token
import Text.Lexer.Inchworm.Char


puncs1 
 =      [ '(', ')'
        , '[', ']'
        , '{', '}'
        , '.', ',', ';' ]

puncs2 
 =      [ "[:", ":]", "{:", ":}" ]


-- | Scan a punctuation character.
scanPunctuation  :: Scanner IO Location [Char] (Location, Token)
scanPunctuation   
 = alt  (munchPred (Just 2) matchPunc2  acceptPunc2)
        (from               acceptPunc1)
 where  
        acceptPunc1 c
         | elem c puncs1        = Just $ KPunc [c]
         | otherwise            = Nothing

        matchPunc2 0 c  = elem c ['[', '{', ':']
        matchPunc2 1 c  = elem c [']', '}', ':']
        matchPunc2 _ _  = False

        acceptPunc2 cs
                | elem cs puncs2        = Just $ KPunc cs
                | otherwise             = Nothing
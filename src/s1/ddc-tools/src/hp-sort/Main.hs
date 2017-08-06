
module Main where
import qualified System.Environment     as System
import qualified Data.List              as List
import Data.Function

main
 = do
        [fileName]      <- System.getArgs
        str             <- readFile fileName
        let (lJob : lData : lSample : lValue : ls) = lines str

        putStr  $ unlines
                $ lJob : lData : lSample : lValue :
                        (squash
                        $ (List.sortOn fst)
                        $ collect Nothing [] ls)


collect :: Maybe String -> [String] -> [String] -> [(String, [String])]
collect mHdr acc []
 | Just hdr     <- mHdr
 = [(hdr, reverse acc)]

 | Nothing      <- mHdr
 = []

collect mHdr acc (s : ss)
 | List.isPrefixOf "BEGIN_SAMPLE" s
 = collect (Just s) [] ss

 | Just hdr     <- mHdr
 , List.isPrefixOf "END_SAMPLE" s
 = let  rest    = collect Nothing [] ss
   in   (hdr, reverse (s : acc)) : rest

 | Just hdr     <- mHdr
 = collect mHdr (s : acc) ss

 | otherwise
 = error $ "unknown: " ++ s


squash :: [(String, [String])] -> [String]
squash ss
 = concatMap (\(hdr, body) -> hdr : body) ss


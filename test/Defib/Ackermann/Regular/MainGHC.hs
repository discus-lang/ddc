
-- $Id: ackermann-ghc-2.code,v 1.1 2006/01/16 01:23:04 bfulgham Exp $
-- http://shootout.alioth.debian.org/
-- shortened by Bryn Keller and Einar Karttunen

import System(getArgs)

-- main = do ~[num] <- getArgs
--	  putStrLn ("Ack(3," ++ num ++ "): " ++ (show (ack 3 (read num))))


ack :: Int -> Int -> Int
ack 0 n = n+1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1));


-----
main 
 = let	num = "9"
   in   putStrLn ("Ack(3," ++ num ++ "): " ++ (show (ack 3 (read num))))

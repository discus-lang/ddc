module Util.Math
 	( mread
	, clamp0
	, decimateR
	, hack
	, shuffle )
where


clamp0 :: (Num a, Ord a) =>	a -> a
clamp0	  a
	= if a < 0 then 0 else a


mread :: Read a => String -> Maybe a
mread	 ss
 = case reads ss of
 	[(a, rest)]	-> Just a
	[]		-> Nothing
		
	

shuffle	::	[a]	-> [a]
shuffle		xx	= hack [xx]

dropEmpties :: [[a]]	-> [[a]]
dropEmpties    []	= []
dropEmpties    (x:xs)	=
 case x of 
  []			-> dropEmpties xs
  otherwise		-> x : dropEmpties xs


hack   :: [[a]]		-> [a]
hack	  []		= []
hack	  xx		= a ++ hack rest
 where
 	(a, rest)	= mash xx

mash   :: [[a]] 	-> ([a], [[a]])
mash 	  xx		= (os, dropEmpties (as ++ bs))
 where   
 	(os, as, bs)	= unzip3 $ map cleave xx


cleave :: [a]	-> (a, [a], [a])
cleave    []	 	= error "cleave: empty list"
cleave    (x:[])	= (x, [], [])
cleave    xx     	= (head b, a, tail b)
 where
	l		= length xx `div` 2
 	(a, b)		= splitAt l xx


decimateR :: Int -> [a] -> ([a], [a])
decimateR    d      xx
 = decimateR' d xx 0 [] []

decimateR' d []     n accD accR	= (accD, accR)
decimateR' d (x:xs) n accD accR 
 | n == 0	= decimateR' d xs (n+1) (x : accD) accR
 | n == d	= decimateR' d xs 0     accD       (x : accR)
 | otherwise	= decimateR' d xs (n+1) accD       (x : accR)
 



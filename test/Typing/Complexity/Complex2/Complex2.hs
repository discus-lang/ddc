
complex2 
 = let	x1	= \y -> \z -> z y y
 	x2	= \y -> x1 (x1 y)
	x3	= \y -> x2 (x2 y)
	x4	= \y -> x3 (x3 y)
	x5	= \y -> x4 (x4 y)
    in	x5 (\z -> z)

 	
main	= putStr "ook";

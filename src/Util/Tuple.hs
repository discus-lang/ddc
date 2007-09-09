
module Util.Tuple
(
	t5App, t5_1, t5_2, t5_3, t5_4, t5_5,
	t4App, t4_1, t4_2, t4_3, t4_4,
	t3App, t3_1, t3_2, t3_3,
	t2App, t2_1, t2_2,
)

where

-----
t5App	(f,   g,   h,   i,   j)   a
 =	(f a, g a, h a, i a, j a)

t5_1	(a, b, c, d, e)	= a
t5_2	(a, b, c, d, e)	= b
t5_3	(a, b, c, d, e)	= c
t5_4	(a, b, c, d, e)	= d
t5_5	(a, b, c, d, e)	= e


-----
t4App	(f,   g,   h,   i)   a
 =	(f a, g a, h a, i a)

t4_1	(a, b, c, d)	= a
t4_2	(a, b, c, d)	= b
t4_3	(a, b, c, d)	= c
t4_4	(a, b, c, d)	= d


-----
t3App	(f,   g,   d)	a
 = 	(f a, g a, d a)
 
t3_1	(a, b, c)	= a
t3_2	(a, b, c)	= b
t3_3	(a, b, c)	= c


-----
t2App	(f,   g)	a
 = 	(f a, g a)
 
t2_1	(a, b)		= a
t2_2	(a, b)		= b


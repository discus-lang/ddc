
module Util.Tunnel
(
	Tunnel,
	(##),
	($<),
	(#!),
	(<#>),  (<#),
	(<##>), (<##),

	getst,

	tl2_1, tl2_2,
	tl3_1, tl3_2, tl3_3,

	mapUpdateTl
)

where

import Control.Monad.State.Strict

type Tunnel s a	
	= ( s -> a
	  , a -> s -> s)

infixr 2 ##
infixr 1 #!, <#, <#>, <##>, <##
infixl 0 $< 

-- Tunnel composition.
(##)	:: Tunnel a b ->	Tunnel b c 	-> Tunnel a c
(##)	   (get1, set1)   	(get2, set2)	

	= ( \a    -> get2 $ get1 a
	  , \c a  -> let
	  		b	= get1 a
			b'	= set2 c  b
			a'	= set1 b' a
		     in a')

-- application
($<) x f	= f x

-- get from tunnel
(#!) ::		a -> Tunnel a b -> b
(#!)	   	a (get, set) 	
	= get a
	
-- set to tunnel
(<#) ::		Tunnel a b -> b -> a -> a
(<#)	   	(get, set)    b    a 
	=  set b a

-- modify via tunnel
(<#>) ::	Tunnel a b -> (b -> b) -> a -> a
(<#>)	   	(get, set)     f          a
 	= set (f (get a)) a
	
-----------------------
-- via state monad
--
-- (<##>) :: 	Tunnel a b -> (b -> b) -> State a ()
(<##>)     	(get, set)    f
 	= modify  (\s -> set (f $ get s) s)

-- (<##)  ::	Tunnel a b -> b		-> State a ()
(<##)		(get, set)    b
	= modify  (\s -> set b s)

getst ::	Tunnel a b -> State a b
getst		(get', set')
 = do
 	a	<- get
	return	(get' a)

-----
-- Tuple2
--
tl2_1 :: Tunnel (a, b) a
tl2_1	= ( \(a, b)	 	-> a
	,   \a' (a, b) 		-> (a', b))

tl2_2 :: Tunnel (a, b) b
tl2_2	= ( \(a, b) 		-> b
	,   \b' (a, b) 		-> (a,  b'))


-----
-- Tuple3
--
tl3_1 :: Tunnel (a, b, c) a
tl3_1	= (\(a, b, c) 		-> a
	,  \a' (a, b, c)	-> (a', b, c))

tl3_2 :: Tunnel (a, b, c) b
tl3_2	= (\(a, b, c) 		-> b
	,  \b' (a, b, c)	-> (a, b', c))
	
tl3_3 :: Tunnel (a, b, c) c
tl3_3	= (\(a, b, c) 		-> c
	,  \c' (a, b, c)	-> (a, b, c'))




-----
--
mapUpdateTl :: Tunnel a b -> (b -> b) -> [a] -> [a]
mapUpdateTl    (get, set)     f          xx
 = case xx of
 	[]	-> []
	(x:xs)	-> set (f (get x)) x : mapUpdateTl (get, set) f xs
	


-----------------------
-- examples
--
{-
type S1	= (Int, Int, S2)
type S2 = (Char, S3, Char)
type S3 = (String, String, String)


t3_1 	= ( \(a, b, c) 	  -> a
	  , \a' (a, b, c) -> (a', b, c))
	  
t3_2 	= ( \(a, b, c) 	  -> b
	  , \b' (a, b, c) -> (a, b', c))

t3_3 	= ( \(a, b, c) 	  -> c
	  , \c' (a, b, c) -> (a, b, c'))


s1	= ( 1
          , 2
	  , ( 'c'
	    , ( "fish"
	      , "rabbit"
	      , "perch")
	    , 'd'))


-- apply function to component
ex1	= s1 $< t3_3 ## t3_2 ## t3_3 
		<#> (\s -> s ++ " fish") 

-- update components
ex2	= s1 $< t3_3 ## t3_2 ## t3_1 
		<# "a"

-- select component
ex3	= s1 #! t3_3 ## t3_2 ## t3_1 
-}


module Source.DefixApps
	(defixApps)

where

import Source.Pretty
import qualified Shared.Var	as Var
import qualified Shared.VarPrim	as Var
import Shared.Var		(Var)

import Shared.Error
import Shared.Base

import Source.Exp
import Util

-----
stage	= "Source.DefixApps"
none	= NoSourcePos

-----------------------
-- defixApps
--	Takes the list of expressions from inside an $XDefix and 
--	builds (suspended) function applications.
--
--
defixApps ::	SourcePos -> [Exp] -> [Exp]
defixApps	sp xx
	= rewriteApps $ dropApps sp xx

-----
-- dropApps
--	Takes a list of expressions from inside an $XDefix and wraps runs of exps that
--	lie between between (non-@) infix operators in $XDefixApps. This is the first
--	step in defixing process.
--
--	eg  [f, x, @, a, b, +, g, y, -, h, @, 5]
--
--	=>  [$XDefixApps [f, x, @, a, b], +, $XDefixApps [g, y], -, XDefixApps [h, @, 5]]
--
dropApps :: 	SourcePos -> [Exp] -> [Exp]
dropApps sp es	

	-- Check if the expression starts with a unary minus
	--	- x + 5   parses as  (negate x) + 5
	| XOp sp v : e2 : esRest	<- es
	, Var.name v == "-"
	= dropApps' sp [XApp sp (XVar sp Var.primNegate) e2] esRest
	
	| otherwise
	= dropApps' sp [] es

-- Collect up pieces of applications in the accumualtor a1 a2 a3 
dropApps' sp acc []	
	= [makeXDefixApps sp acc]

dropApps' sp acc xx
	-- Chop out the unary minus if we see two ops in a row
	--	x1 += - f x 	parses as   x1 += (- (f x))
	| x1@(XOp sp1 v1) : x2@(XOp sp2 v2) : xsRest	<- xx
	, Var.name v2 == "-"
	= makeXDefixApps sp acc : x1 
	: dropApps' sp [XVar sp Var.primNegate] xsRest
	
	
	-- when we hit a non '@' operator, mark the parts in the accumulator
	--	as an application and start collecting again.
 	| x@(XOp sp v) : xs				<- xx
	, Var.name v /= "@"
	= makeXDefixApps sp acc : x : dropApps' sp [] xs

	| x : xs	<- xx
	= dropApps' sp (acc ++ [x]) xs

makeXDefixApps sp xx	
 = case xx of
	-- If we hit two operators in a row, and the second one isn't unary 
	--	minus then we'll get an empty list here. eg  x + + 1
	[]	-> panic stage 
			$ "makeXDefixApps: parse error at\n" % sp
			% "   xx = " % xx	% "\n"	
	[x]	-> x
	_	-> XDefixApps sp xx


-----
-- rewriteApps
--	Takes a list of expressions and converts $XDefixApp nodes into XApp nodes.
--	Also converts @ operators into explicit calls to the suspend functions.
--
--	eg [f, x, @, a, b, @, c, d, e]
--
--	=> suspend3 (suspend2 (f x) a b) c d e
--
--
rewriteApps ::	[Exp] 	-> [Exp]
rewriteApps	[]	= []
rewriteApps	(x:xs)
 = case x of
 	XDefixApps sp xx
	 -> rewriteApp sp xx : rewriteApps xs
	 
	_ -> x : rewriteApps xs
	
	
rewriteApp :: 	SourcePos -> [Exp] -> Exp
rewriteApp	sp es	
	= rewriteApp' sp [] es

rewriteApp' sp left []
 	= unflattenApps sp left

rewriteApp' sp left (x:xs)
 = case x of
 	XOp sp op
	
	 -- If dropApps is working properly then we shouldn't find
	 --	any non-@ operators at this level. 
	 |  Var.name op /= "@"
 	 -> panic stage "rewriteApp: found non-@ operator."
	 
	 |  otherwise
	 -> let (bits, rest)	= takeUntilXOp [] xs
		args		= length bits

		suspV		= Var.primSuspend args
		susp		= suspV
				{ Var.info 		= (Var.info suspV) ++ (Var.info op) }
	 	 
		leftApp		= unflattenApps sp left
		app		= unflattenApps sp (XVar sp susp : leftApp : bits)
	 	
	   in	rewriteApp' sp [app] rest
		
	_ -> rewriteApp' sp (left ++ [x]) xs
	 	

takeUntilXOp acc []	= (acc, [])
takeUntilXOp acc (x:xs)
 = case x of
 	XOp{}		-> (acc, x:xs)
	_		-> takeUntilXOp (acc ++ [x]) xs
 	

 
flattenApps  :: Exp	-> [Exp]
flattenApps x
 = case x of
	XApp sp x1 x2 	-> x1 : flattenApps x2
	e		-> [e]	
 

unflattenApps :: SourcePos -> [Exp] -> Exp
unflattenApps	sp [x]	= x
unflattenApps  	sp xx	
 = unflattenApps' sp $ reverse xx

unflattenApps' sp xx
 = case xx of
	(x1:x2:[])	-> XApp sp x2 x1
 	(x:xs)		-> XApp sp (unflattenApps' sp xs) x
	



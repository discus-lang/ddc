
-- Core.Prim
--	Find direct uses of primitive functions and replace them by XPrim nodes.
--	TODO: do fast unboxing if the data is direct
--
module Core.Prim
	( primTree )


where

import Util
import Core.Exp
import Core.Util
import Core.Reconstruct
import Core.Plate.Trans


import Data.Map			(Map)
import qualified Data.Map	as Map

import qualified Shared.Var	as Var
import Shared.VarPrim

import qualified Debug.Trace

debug	= False
trace ss x
 = if debug 
 	then Debug.Trace.trace (pprStr ss) x
  	else x

-----
stage	= "Core.Prim"

-- Identify primitive operations
--	Tree should be in snipped form
primTree :: Tree -> Tree
primTree tree
	= transformS primS tree

-- Identify a primitive operation in this statement
--	We go via statements, in snipped form so its easy to identify the entire application expression

--	TODO: 	fix this for over-application
--		need to split off some of the args

primS ss
 = case ss of
	SBind mV (XTau t x)	-> SBind mV (XTau t $ primX x)
 	SBind mV x		-> SBind mV (primX x)
	
primX xx
	-- direct use of boxing function
	| Just parts				<- flattenAppsEff xx
	, (XVar v t : psArgs)			<- parts
	, isBoxFunctionV v
	= XPrim MBox psArgs

	-- direct use of unboxing function
	| Just parts				<- flattenAppsEff xx
	, (XVar v t : psArgs)			<- parts
	, isUnboxFunctionV v
	= XPrim MUnbox psArgs

	-- look for functions in the prim table
 	| Just parts				<- flattenAppsEff xx
	, (XVar v t : psArgs)			<- parts
	, Just operator				<- Map.lookup (Var.name v) primFuns
	= XPrim (MOp operator) psArgs
	
	-- look for functions who's arguments can be unboxed
 	| Just parts				<- flattenAppsEff xx
	, (XVar v t : psArgs)			<- parts
	, Just (operator, actions)		<- Map.lookup (Var.name v) unboxableFuns
	, tResult@(TData vResult tsResult)	<- reconX_type (stage ++ ".primX") xx
	, length psArgs == length actions
	= trace ( "primX:\n"
		% "    xx      = " % xx 	% "\n"
		% "    parts   = " % parts	% "\n"
		% "    tResult = " % tResult	% "\n") 
	   $ let	
		-- apply the actions to the arguments
		psArgs'	= doActions actions psArgs
		
		-- generate the primitive 
	   in	XPrim MBox (map XType tsResult ++ [XPrim (MOp operator) psArgs'])
	
	| otherwise		
	= xx

-- | Perform these actions on this list of expressions
doActions :: [Action] -> [Exp] -> [Exp]
doActions (Discard:as) (x:xs)
	= doActions as xs

doActions (Ignore:as) (x:xs)
	= x : doActions as xs
	
doActions (Unbox:as) (x:xs)
 = let	tX@(TData v [r])	
 		= reconX_type (stage ++ "doActions") x
	x'	= XPrim MUnbox [XType r, x] : doActions as xs

   in	trace	( "doActions: Unbox\n"
		% "    x    = " % x 	% "\n"
		% "    tX   = " % tX	% "\n")
		$ x'
   
	
		
doActions _ xs
	= xs

-- | Flatten an application into its component parts, left to right
flattenAppsEff :: Exp -> Maybe [Exp]
flattenAppsEff xx
 = case xx of
 	XApp{}	-> Just $ flattenAppsEff' xx
	XAPP{}	-> Just $ flattenAppsEff' xx
	_	-> Nothing

flattenAppsEff' xx
 = let	bits	= flattenAppsE xx
 	
	(parts, effss)
		= unzip
		$ map (\p -> case p of
				XAppFP x mEff	-> (x, mEff))
		$ bits
    in	parts
	

-- whether to ignore or unbox the argument
data Action
	= Ignore
	| Discard
	| Unbox
	deriving (Show, Eq)

-- poor man's type signatures
--	Says what to do with the arguments of a function.
--	We tend to ignore type arguments, and unbox boxed values.
opI3U2	= [Ignore, Ignore, Ignore, Unbox, Unbox]
opD3U2	= [Discard, Discard, Discard, Unbox, Unbox]

----------------------------------------------------------------------------------------------------
-- TODO: the code for these tables could be a lot cleverer
--	explicitly listing each member won't be the right way when we want
--	to support all the other primitive types as well.


-- functions who's arguments can be unboxed
unboxableFuns :: Map String (Op, [Action])
unboxableFuns
 = Map.fromList

	-- boxed int functions
	[ ("primInt32_add",	(OpAdd,	opD3U2))
	, ("primInt32_sub",	(OpSub,	opD3U2))
	, ("primInt32_div",	(OpDiv,	opD3U2))
	, ("primInt32_mod",	(OpMod,	opD3U2))
	, ("primInt32_mul",	(OpMul,	opD3U2))

	-- boxed comparisions
	, ("primInt32_eq",	(OpEq,	opD3U2))
	, ("primInt32_neq",	(OpNeq,	opD3U2))
	, ("primInt32_gt",	(OpGt,	opD3U2))
	, ("primInt32_ge",	(OpGe,	opD3U2))
	, ("primInt32_lt",	(OpLt,	opD3U2))
	, ("primInt32_le",	(OpLe,	opD3U2))

	-- boxed boolean functions
	, ("&&",		(OpAnd,	opD3U2))
	, ("||",		(OpOr,	opD3U2))

	-- boxed float functions
	, ("primFloat32_add",	(OpAdd,	opD3U2))
	, ("primFloat32_sub",	(OpSub,	opD3U2))
	, ("primFloat32_div",	(OpDiv,	opD3U2))
	, ("primFloat32_mul",	(OpMul,	opD3U2))
	, ("primFloat32_eq",	(OpEq,	opD3U2))
	, ("primFloat32_neq",	(OpNeq,	opD3U2))
	, ("primFloat32_gt",	(OpGt,	opD3U2))
	, ("primFloat32_ge",	(OpGe,	opD3U2))
	, ("primFloat32_lt",	(OpLt,	opD3U2))
	, ("primFloat32_le",	(OpLe,	opD3U2)) ]
	

-- primitive functions
primFuns :: Map String Op
primFuns 
 = Map.fromList

	-- unboxed int functions
 	[ ("primInt32U_add",	OpAdd)
	, ("primInt32U_sub",	OpSub)
	, ("primInt32U_div",	OpDiv)
	, ("primInt32U_mod",	OpMod)
	, ("primInt32U_mul",	OpMul) 

	-- unboxed int comparisons
	, ("primInt32U_eq",	OpEq)
	, ("primInt32U_neq",	OpNeq)
	, ("primInt32U_gt",	OpGt)
	, ("primInt32U_ge",	OpGe)
	, ("primInt32U_lt",	OpLt)
	, ("primInt32U_le",	OpLe) 
	
	-- unboxed float functions
	, ("primFloat32U_add",	OpEq)
	, ("primFloat32U_sub",	OpSub)
	, ("primFloat32U_div",	OpDiv)
	, ("primFloat32U_mod",	OpMod)
	, ("primFloat32U_mul",	OpMul) 

	-- unboxed int comparisons
	, ("primFloat32U_eq",	OpEq)
	, ("primFloat32U_neq",	OpNeq)
	, ("primFloat32U_gt",	OpGt)
	, ("primFloat32U_ge",	OpGe)
	, ("primFloat32U_lt",	OpLt)
	, ("primFloat32U_le",	OpLe) ]




isUnboxFunctionV :: Var -> Bool
isUnboxFunctionV v
 =  elem (Var.name v)
 	[ "unboxInt32"
	, "unboxFloat32" ]

isBoxFunctionV :: Var -> Bool
isBoxFunctionV v
 = elem	(Var.name v)
 	[ "boxInt32"
	, "boxFloat32" ]
	

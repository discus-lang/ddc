-- | Resolves calls to overloaded functions.

module Core.Dictionary
	( dictTree )

where

import Core.Exp
import Core.Pretty
import Core.Util
import Core.Plate.Trans
import Core.Util.Unify
import Core.ReconKind

import Core.Util.Strip
import Core.Util.Substitute
import Core.Util.Slurp
import Core.Pretty

import qualified Debug.Trace	as Debug
import Shared.Error
import Shared.Pretty

import qualified Data.Map	as Map
import qualified Util.Map	as Map
import Data.Map			(Map)

import Util

-----
stage		= "Core.Dictionary"
debug		= False
trace ss x	= if debug 
			then Debug.trace ss x
			else x

-----
--	The RHS of every instance function must be the var of a top level function.

dictTree 
	:: Tree
	-> Tree 	
	-> Tree
dictTree hTree sTree
 = let	instMap		= slurpClassInstMap 		(sTree ++ hTree)
 	classMap	= slurpClassFuns instMap 	(sTree ++ hTree)
	typeMap		= slurpTypeMapPs		(sTree ++ hTree)

	tree'		= transformS rewriteS sTree
				where 	?typeMap	= typeMap
					?classMap	= classMap
   in	tree'
   	
	
-- | Rewrite overloaded fn applications in this statement
rewriteS
 ::	(?typeMap 	:: Map Var Type)
 ->	(?classMap 	
 	 :: Map Var 			-- overloaded variable, eg (+)
		( Class			-- class that the var belongs to, eg (Num a)
		, Type			-- type of this overloaded function
					--	eg Num a => a -> a -> a
		, [(Class, Var)]))	-- the possible instances for this function
					--	eg Int   -> Int -> Int
					--	   Float -> Float -> Float

 ->	Stmt -> Stmt
 
rewriteS ss
 = case ss of
 	SBind mV x	-> SBind mV $ rewriteX x
	
rewriteX xx
 = case xx of
	XTau t x	-> XTau t (rewriteX x)
	XApp{}		-> rewriteApp xx
	XAPP XLit{} (TVar KRegion r) -> xx
	XAPP{}		-> rewriteApp xx
	_		-> xx
	

-- | if this function application calls an overloaded function then rewrite it 
--	to be a call to the appropriate instance.
rewriteApp xx
 = let	
 	-- flatten the application to extract the var of the function being applied.
 	xParts				= flattenAppsE xx
	(XAppFP (XVar overV _) _ : _)	= xParts

 	result
		-- see if the var is in the classMap, if it is then it is overloaded.
		| Just	( cClass@(TClass vClass tsClass)
			, tOverScheme
			, cxInstances)	
				<- Map.lookup overV ?classMap
		
		= let	vksClass 	= map (\(TVar k v) -> (v, k)) tsClass
			tScheme	 	= foldl (\t (v, k) -> TForall (BVar v) k t) tOverScheme vksClass
		  	tScheme_c	= addContext (KClass vClass tsClass) tScheme
		  
		  in 	rewriteOverApp_trace xx overV cClass tScheme_c cxInstances ?typeMap
		
		-- couldn't find it in the classMap, var isn't overloaded.
		| otherwise
		= xx
	
   in	result

addContext c tt
 = case tt of
 	TForall b k t	-> TForall b k (addContext c t)
	_		-> TContext c tt

-- rewriteOverApp ----------------------------------------------------------------------------------	
-- | Rewrite an overloaded application to call the appropriate instance.
rewriteOverApp
	:: Exp			-- function application to rewrite
	-> Var			-- var of overloaded function
	-> Class		-- class 		eg	Eq a
	-> Type			-- type of sig
	-> [(Class, Var)]	-- instances		eg 	[(Eq Int, primIntEq), (Eq Char, primCharEq)]
	-> Map Var Type		-- type map
	-> Exp			-- rewritten expression

rewriteOverApp
	xx 
	overV 
	cClass@(TClass classV classParamVs) 
	tOverScheme 
	cxInstances
	mapTypes

	-- first work out what instance function to use
	| (cClassInst, tOverShapeI, Just vInst)
			<- determineInstance xx overV cClass tOverScheme cxInstances 

	-- Ok, we know what types the overloaded function was called at, and what instance to use.
	--	We now have to work out what type args to pass to the instance function.

	-- Lookup the scheme for the instance function and strip out its parts
	, Just tInstScheme	<- Map.lookup vInst mapTypes

	, (bksForall, _, ksClass, tInstShape)
			<- stripSchemeT $ packT tInstScheme					
	
	= trace (pprStrPlain
		$ "    tInstScheme = " %> tInstScheme 	% "\n\n"
		% "    tInstShape  = " %> tInstShape 	% "\n\n"
		% "    tOverShapeI = " %> tOverShapeI	% "\n\n")
	$ let
		-- Match up the type args in the type of the call and the real instance function.
		ttSub	= fromMaybe
				(panic stage $ "rewriteOverApp: cannot unify types.\n\n"
				% "    tInstShape  = " %> tInstShape  % "\n\n"
				% "    tOverShapeI = " %> tOverShapeI % "\n")
				$ unifyT2 tInstShape tOverShapeI

		-- Make sure we have a type parameter for all free vars in the instance.
		-- If this fails then the type params passed to the overloaded fn weren't specific enough
		--	to determine the type we need to call the instance fn at.
		vtSub	= map 	(\(t1, t2) -> 
					let Just v1	= typeToVar t1
					in  (v1, t2))
				ttSub

		-- Work out the type args to pass to the instance function.
		getInstType (b, k)
		 = case lookup (varOfBind b) vtSub of
		 	Just t'	-> t'

			-- if the instance is recursive then vtSub will be empty and we can just
			--	pass the vars back to ourselves.
			Nothing	
			 | null vtSub	-> TVar k (varOfBind b)
		 
		tsInstArgs	
		 	= trace (pprStrPlain $ "    vtSub:\n" %> vtSub % "\n")
		 	$ map getInstType bksForall

		-- Work out the witnesses we need to pass to the instance function
		ksClassArgs		= map (\c -> substituteT (Map.fromList vtSub) c) ksClass
		Just tsWitnesses	= sequence $ map takeWitnessOfClass ksClassArgs

		-- Have a look at the original application 
		--	split off the type/class args and keep the value args.
		(XAppFP (XVar _ _) _ : xsArgs)	
				= flattenAppsE xx
	
		(vtsForallO, _, csClassO, _)
				= stripSchemeT tOverScheme
	
		(_, xsArgsVal)	= splitAt 
					(length vtsForallO + length csClassO)
					 xsArgs
	
		-- Construct the new call.
		xxParts' :: [Exp]
	  	xxParts'
			= ((XAppFP (XVar vInst tInstScheme) Nothing)			-- the function var
				:  (map (\t -> XAppFP (XType t) Nothing) tsInstArgs)	-- type args to instance fn
				++ (map (\t -> XAppFP (XType t) Nothing) tsWitnesses)	-- class args to instance fn
				++ xsArgsVal)						-- value args

	  	xx'	= unflattenAppsE xxParts'

   	in trace
		(pprStrPlain
			$ "rewriteOverApp/leave\n"
			% "  tInstScheme        = \n" %> tInstScheme	% "\n\n"
			% "  vtSub              = " % vtSub		% "\n\n"
			% "  tsInstArgs         = " % tsInstArgs	% "\n\n"
			% "  xx'                =\n" %> xx'		% "\n\n")
		$ xx'

	-- couldn't find the instance for this function			
	| (cClassInst, tOverShapeI, Nothing)
    		<- determineInstance xx overV cClass tOverScheme cxInstances
	= panic stage 
		$ "rewriteOverApp: no instance for " % cClassInst % "\n"
		% "  in " % xx % "\n"


rewriteOverApp_trace
	xx overV cClass tOverScheme cxInstances mapTypes
 =  trace
	(pprStrPlain
		$ "* rewriteOverApp_trace/enter\n"

		% "    function application to rewrite (xx)\n"
			%> (" = " % xx		% "\n\n")

		% "    overloaded var (overV)\n"		
			%> (" = " % overV	% "\n\n")

		% "    type class of overloaded var (cClass)\n"
			%> (" = " % cClass	% "\n\n")

		% "    scheme of overloaded var (tOverScheme)\n"
			%> (" = " % tOverScheme	% "\n\n")

		% "    possible instance for this var (cxInstance)\n"
			%> (punc "\n" cxInstances	% "\n\n"))
		
	$ rewriteOverApp xx overV cClass tOverScheme cxInstances mapTypes


-- determineInstance -------------------------------------------------------------------------------
-- | Determine which instance to use for this application of an
--	overloaded function.
determineInstance
	:: Exp			-- ^ fn application being rewritten
	-> Var			-- ^ var of overloaded
	-> Class		-- ^ type class of the overloaded
	-> Type			-- ^ scheme of overloaded
	-> [(Class, Var)]	-- ^ possible instances for this fn

	-> ( Class		-- the instance we need
	   , Type		-- shape of overloaded scheme, once the type args have been applied to it.
	   , Maybe Var)		-- var of the instance function (if found)
		
determineInstance 
	xx 		
	overV 		
	cClass@(TClass vClass tsClassParams)	
	overScheme	
	cvInstances	
 = let
 	-- See how many foralls there are on the front of the overloaded scheme.
	(vtsForall, _, _, tOverShape)
			= stripSchemeT overScheme

	vsForall	= map fst vtsForall
	vsForall_count	= length vsForall

	-- Split out enough args to saturate all the foralls.
	(_ : xsArgs)	= flattenAppsE xx
	(xsArgsQuant, _)
			= splitAt vsForall_count xsArgs

	tsArgsQuant	= map (\(XAppFP (XType t) _) -> t) xsArgsQuant
	
	-- Substitute the bound types into the context
	tsSubst		= Map.fromList $ zip (map varOfBind vsForall) tsArgsQuant
	tsClassParams'	= map (substituteT tsSubst) tsClassParams
	cClassInst	= TClass vClass tsClassParams'

	-- Lookup the instance fn
	mInstV		= lookupF matchInstance cClassInst cvInstances

	-- Also apply types to the shape of the overloaded fn to get the 
	--	shape of the instance.
	tOverShapeI	= substituteT tsSubst tOverShape

 in	trace
 		(pprStrPlain
			$ "* determineInstance\n"
			% "    subst these type args into the class context (tsSubst)\n"
				%> ("  = " % tsSubst % "\n\n")

			% "    the context after substitution (cClassInst)\n"
				%> (" = "  % cClassInst	% "\n\n")

			% "    the available instances (cvInstances)\n"
				%> (punc "\n" cvInstances % "\n\n")

			% "    var of the instance fn to use (mInstV)\n"
				%> (" = "  % mInstV	% "\n\n")

			% "    shape of the overloaded fn (tOverShape)\n"
				%> (" = "  % tOverShape % "\n\n")

			% "    shape of the overloaded fn at this instance (tOverShapeI)\n"
				%> (" = "  % tOverShapeI % "\n\n"))

		$ (cClassInst, tOverShapeI, mInstV)


-- Checks if an class instance supports a certain type.
--	The class instance has to be more polymorphic than the type we want to support.
matchInstance 
	:: Type 	-- the class we want to support
	-> Type		-- the class of the instance
	-> Bool

matchInstance cType cInst
	| TClass v1 ts1		<- cType
	, TClass v2 ts2		<- cInst

	-- check the class is the same
	, v1 == v2
	, length ts1 == length ts2

	-- all the type arguments of the class must unify
	, Just constrs		<- sequence $ zipWith unifyT2 ts1 ts2

	-- any extra constraint from the unification must have 
	--	a var or wildcard for the RHS
	, and 	$ map (\(ta, tb) -> case tb of
				TVar  k v
				 | kindOfType ta == k
			 	 -> True

				TWild k
				 | kindOfType ta == k
				 -> True

				_	-> False)

		$ concat $ constrs
	= True

	| otherwise
	= False


-- Slurp -------------------------------------------------------------------------------------------
-- Slurp out a map of all overloaded functions defined in this tree.
slurpClassFuns 
 ::	Map Var [Top]
 -> 	[Top]	

 -> Map Var 			-- name of overloaded function
 	( Class			-- the class that this function is in
	, Type			-- type of the overloaded function
	, [(Class, Var)])	-- instances

slurpClassFuns instMap pp
 = Map.fromList
 	[ (vF, (TClass v ts, sig, exps))

	| PClassDict v ts context sigs	<- pp 
	, (vF, sig)		<- sigs 
	, let (Just insts)	= Map.lookup v instMap
	, let exps		= [ (TClass v' ts', instV)	
					| PClassInst v' ts' _ defs	<- insts
					, (v, (XVar instV t))		<- defs
					, v == vF		] ]

-- | Slurp out all the class instances from ths tree
slurpClassInstMap
 ::	Tree	-> Map Var [Top]

slurpClassInstMap tree
 = 	Map.gather
 	[ (v, p)	| p@(PClassInst v ts context defs) <- tree]



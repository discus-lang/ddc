
module Core.Dictionary
(
	dictTree
)

where

import qualified Data.Map	as Map
import qualified Util.Map	as Map
import Data.Map			(Map)

import qualified Debug.Trace	as Debug
import Shared.Error

import Util
import Core.Exp
import Core.Pretty
import Core.Util
import Core.Plate.Trans
import Core.Util.InlineTLet
import Core.Util.Unify

import Core.Util.Strip
import Core.Util.Substitute
import Core.Util.Slurp
import Core.Pretty

stage		= "Core.Dictionary"
debug		= True
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
   	
	
		
-----
-- 
rewriteS
 ::	(?typeMap 	:: Map Var Type)
 ->	(?classMap 	:: Map Var (Class, Type, [(Class, Var)]))
 ->	Stmt -> Stmt
 
rewriteS ss
 = case ss of
 	SBind mV x	-> SBind mV $ rewriteX x
	_		-> ss
	
rewriteX xx
 = case xx of
 	XTau t x	-> XTau t (rewriteX x)
	XApp{}		-> rewriteApp xx
	XAPP{}		-> rewriteApp xx
	_		-> xx
	
rewriteApp xx
 = let	xParts				=  flattenAppsE xx
	(XAppFP (XVar overV) _ : _)	= xParts
	
   in	case Map.lookup overV ?classMap of
   	 Nothing		-> xx
	 Just (cClass, tOverScheme, cxInstances)	
	 	-> rewriteOverApp_trace xx overV cClass tOverScheme cxInstances ?typeMap

		
rewriteOverApp
 ::	Exp			-- expression being rewritten
 ->	Var			-- var of overloaded function
 ->	Class			-- class 		eg	Eq a
 -> 	Type			-- type of sig
 -> 	[(Class, Var)]		-- instances		eg 	[(Eq Int, primIntEq), (Eq Char, primCharEq)]
 ->	Map Var Type		-- type map
 -> 	Exp			-- rewritten expression

-- ^ Rewrite an overloaded application to call the appropriate instance.

rewriteOverApp
	xx 
	overV 
	cClass@(TClass classV classParamVs) 
	tOverScheme 
	cxInstances
	mapTypes

	-- See if we can find an instance for this overloaded function
 	| (Just vInst, tOverShapeI)
			<- determineInstance xx overV cClass tOverScheme cxInstances
	= let
		-- Ok, we know what types the overloaded was called at, and what instance function to call.
		--	We now have to work out what type args to pass to the instance function.
		
		-- Lookup a scheme for the instance function
		Just tInstScheme= Map.lookup vInst mapTypes

		-- Strip this scheme down to its shape
		(vtsForall, vtsTet, csClass, tInstShape)
				= stripSchemeT tInstScheme					

		-- Unify the instance shape with the overloaded shape.
		ttSub		= trace (pretty $ "  tInstShape = " % tInstShape % "\n")
				$ fromMaybe
					(panic stage $ "rewriteOverApp: cannot unify types.\n\n"
					  % " tInstShape  = " %> tInstShape  % "\n\n"
					  % " tOverShapeI = " %> tOverShapeI % "\n")
					$ unifyT2 tInstShape tOverShapeI
		

		vtSub		= trace (pretty $ "  ttSub      = " % ttSub)
				$ map 	(\(t1, t2) -> 
						let Just v1	= typeToVar t1
						in  (v1, t2))
					ttSub
		
		-- Work out the type args to pass to the instance function.
		vsForall	= map fst vtsForall
		tsInstArgs	= map (\v -> let Just t = lookup v vtSub in t) 	vsForall
		tsClassArgs	= map (\c -> c)					csClass

		-- Have a look at the original application 
		--	split off the type/class args and keep the value args.
		(XAppFP (XVar overV) _ : xsArgs)	
				= flattenAppsE xx
	
		(vtsForallO, _, csClassO, _)
				= stripSchemeT tOverScheme
	
		(_, xsArgsVal)	= splitAt 
					(length vtsForallO + length csClassO)
					 xsArgs
	
		-- Construct the new call.
		xxParts' :: [Exp]
	  	xxParts'	= ((XAppFP (XVar vInst) Nothing)				-- the function var
					:  (map (\t -> XAppFP (XType t) Nothing) tsInstArgs)	-- type args to instance fn
					++ (map (\t -> XAppFP (XType t) Nothing) tsClassArgs)	-- class args to instance fn
					++ xsArgsVal)						-- value args

	  	xx'		= unflattenAppsE xxParts'

   	in trace
		(pretty	$ "rewriteOverApp/leave\n"
			% "  tInstScheme        = \n" %> tInstScheme	% "\n\n"
			% "  vtSub              = " % vtSub		% "\n\n"
			% "  tsInstArgs         = " % tsInstArgs	% "\n\n")
		$ xx'
			


	| otherwise
	=  panic stage 
	$ "rewriteOverApp: no instance for " % cClass % "\n"
	% "  in " % xx % "\n"


rewriteOverApp_trace
	xx overV cClass tOverScheme cxInstances mapTypes
 =  trace
	(pretty	$ "* rewriteOverApp_trace/enter\n"
		% "    xx          = \n"	%> xx				% "\n\n"
		% "    overV       = "		% overV				% "\n"
		% "    cClass      = "		% cClass			% "\n\n"
		% "    tOverScheme = \n"	%> tOverScheme			% "\n\n"
		% "    cxInstances = \n"	%> cxInstances			% "\n\n")
		
	$ rewriteOverApp xx overV cClass tOverScheme cxInstances mapTypes




----
-- determineInstance
--
determineInstance
	:: Exp			-- an application of an overloaded function
	-> Var			-- overloaded fn name.
	-> Class		-- the class which this function is in.
	-> Type			-- overloaded fn scheme.
	-> [(Class, Var)]	-- instances for this class.

	-> ( Maybe Var		-- var of the instance function.
	   , Type)		-- shape of overloaded scheme, once the type args have been applied to it.
				
				

determineInstance 
	xx 		
	overV 		
	cClass@(TClass vClass tsClassParams)	
	overScheme	
	cvInstances	
 = let
 	-- See how many foralls there are on the front of the overloaded scheme.
	(vtsForall, vtsTet, csClass, tOverShape)
			= stripSchemeT overScheme

	vsForall	= map fst vtsForall
	vsForall_count	= length vsForall

	-- Split out enough args to saturate all the foralls.
	(_ : xsArgs)	= flattenAppsE xx
	(xsArgsQuant, xsArgsRest)
			= splitAt vsForall_count xsArgs

	tsArgsQuant	= map (\(XAppFP (XType t) _) -> t) xsArgsQuant
	
	-- Use these type applications to work out what class instance
	--	we're supposed to be using.
	subType		= Map.fromList $ zip vsForall tsArgsQuant
	tsClassParams'	= map (substituteT subType) tsClassParams
	cClassInst	= TClass vClass tsClassParams'

	-- Try and find an instance for the overloaded function in the instance table.
	mInstV		= lookupF matchC2 cClassInst cvInstances

	-- Apply the substitution to the overloaded scheme
	--	We'll use this later to work out how to call the instance.
	tOverShapeI	= substituteT subType tOverShape


 in	trace
 		(pretty	$ "* determineInstance\n"
			% "    subType     = " % subType	% "\n"
			% "    cClassInst  = " % cClassInst	% "\n"
			% "    mInstV      = " % mInstV		% "\n"
			% "    tOverShapeI = " % tOverShapeI	% "\n")
		$ (mInstV, tOverShapeI)




-----
-- slurpClassFuns
--
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
	, (vF, sig)			<- sigs 
	, let (Just insts)		= Map.lookup v instMap
	, let exps			= [ (TClass v' ts', instV)	
							| PClassInst v' ts' _ defs	<- insts
							, (v, (XVar instV))		<- defs
							, v == vF		] ]


-----
-- slurpClassInstMap
--
slurpClassInstMap
 ::	Tree	-> Map Var [Top]
 
slurpClassInstMap tree
 = 	Map.gather
 	[ (v, p)	| p@(PClassInst v ts context defs) <- tree]



		

-- | Resolves calls to overloaded functions.

module Core.Dictionary
	( dictTree )

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
import Core.Util.Unify

import Core.Class.Discharge
import Core.Util.Strip
import Core.Util.Substitute
import Core.Util.Slurp
import Core.Pretty

-----
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
		ttSub	= trace (pretty $ "  tInstShape = " % tInstShape % "\n")
			$ fromMaybe
				(panic stage $ "rewriteOverApp: cannot unify types.\n\n"
				  % " tInstShape  = " %> tInstShape  % "\n\n"
				  % " tOverShapeI = " %> tOverShapeI % "\n")
				$ unifyT2 tInstShape tOverShapeI
		

		vtSub	= trace (pretty $ "  ttSub      = " % ttSub)
			$ map 	(\(t1, t2) -> 
					-- if this fails then the type params passed to the overloaded
					--	fn weren't specific enough to determine the type 
					--	we need to call the instance fn at.
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

		% "    function application to rewrite (xx)\n"
			%> (" = " % xx		% "\n\n")

		% "    overloaded var (overV)\n"		
			%> (" = " % overV	% "\n\n")

		% "    type class of overloaded var (cClass)\n"
			%> (" = " % cClass	% "\n\n")

		% "    scheme of overloaded var (tOverScheme)\n"
			%> (" = " % tOverScheme	% "\n\n")

		% "    possible instance for this var (cxInstance)\n"
			%> (" = " % cxInstances	% "\n\n"))
		
	$ rewriteOverApp xx overV cClass tOverScheme cxInstances mapTypes


-- | Determine which instance to use for this application of an
--	overloaded function.
determineInstance
	:: Exp			-- ^ fn application being rewritten
	-> Var			-- ^ var of overloaded
	-> Class		-- ^ type class of the overloaded
	-> Type			-- ^ scheme of overloaded
	-> [(Class, Var)]	-- ^ possible instances for this fn

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
	
	-- Substitute the bound types into the context
	tsSubst		= Map.fromList $ zip vsForall tsArgsQuant
	tsClassParams'	= map (substituteT tsSubst) tsClassParams
	cClassInst	= TClass vClass tsClassParams'

	-- Lookup the instance fn
	mInstV		= lookupF matchInstance cClassInst cvInstances

	-- Also apply types to the shape of the overloaded fn to get the 
	--	shape of the instance.
	tInstShape	= substituteT tsSubst tOverShape

 in	trace
 		(pretty	$ "* determineInstance\n"
			% "    subst these type args into the class context (tsSubst)\n"
				%> ("  = " % tsSubst % "\n\n")

			% "    the context after substitution (cClassInst)\n"
				%> (" = "  % cClassInst	% "\n\n")

			% "    the available instances (cvInstances)\n"
				%> (" = "  % cvInstances % "\n\n")

			% "    var of the instance fn to use (mInstV)\n"
				%> (" = "  % mInstV	% "\n\n")

			% "    shape of the instance fn (tInstShape)\n"
				%> (" = "  % tInstShape % "\n\n"))

		$ (mInstV, tInstShape)


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
	, (vF, sig)			<- sigs 
	, let (Just insts)		= Map.lookup v instMap
	, let exps			= [ (TClass v' ts', instV)	
							| PClassInst v' ts' _ defs	<- insts
							, (v, (XVar instV))		<- defs
							, v == vF		] ]

-- | Slurp out all the class instances from ths tree
slurpClassInstMap
 ::	Tree	-> Map Var [Top]

slurpClassInstMap tree
 = 	Map.gather
 	[ (v, p)	| p@(PClassInst v ts context defs) <- tree]



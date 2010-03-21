
-- | Works out how to perform function applications and rewrites them into
--   super-call/curry/apply/tail-calls
module Core.Curry
	(curryTree)
where
import Core.Exp
import Core.Util
import Core.Glob
import Type.Util
import Type.Exp
import Shared.Error
import Shared.Pretty
import Util
import Shared.Var		(Var, NameSpace(..))
import Main.Arg			(Arg)
import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var
import qualified Main.Arg	as Arg
import qualified Debug.Trace	as Debug

-----
stage		= "Core.Curry"
debug		= False
trace s	x 	= if debug then Debug.trace (pprStrPlain s) x else x


-- | Environment for this transform
data Env
	= Env
	{ envGlobHeader		:: Glob
	, envGlobProg		:: Glob }


-- | Work out how to perform function applications in this tree
--	rewrite them to use the primitive application operators.
curryTree 
	:: (?args :: [Arg])
	=> Tree			-- ^ header tree
	-> Tree			-- ^ source tree
	-> Glob			-- ^ header glob
	-> Glob			-- ^ source glob
	-> Tree 

curryTree 
	headerTree	
	coreTree
	headerGlob
	progGlob
 = let 	env	= Env headerGlob progGlob
   in   map (curryP env) coreTree

curryP env p
 = case p of
 	PBind v x
	 | elem Arg.OptTailCall ?args
	 -> PBind v (curryX env [v] x)
	 
	 | otherwise
	 -> PBind v (curryX env [] x)
	 
	_ -> p
	

-- | Rewrite function applications in this statement
curryS 	:: Env 
	-> [Var]		-- ^ supers that can be tail-called.
	-> Stmt 
	-> Stmt

curryS env vsTailCall s
 = case s of
	SBind v x	-> SBind v (curryX env vsTailCall x)

   
-- | Rewrite function applications in this expression
curryX	env vsTailCall xx
	-- boilerplate.
	| XLAM v k x		<- xx	= XLAM v k (downX x)
	| XLam v t x eff clo	<- xx	= XLam v t (downX x) eff clo
	| XTau t x		<- xx	= XTau t	$ downX x
	| XLocal v vs x		<- xx	= XLocal v vs	$ downX x
	| XMatch aa		<- xx	= XMatch 	$ map downA aa
	| XPrim{}		<- xx	= xx
	| XLit{} 		<- xx	= xx

	-- A zero arity super.
	| XVar v t		<- xx
	= if   (globDeclaresValue v $ envGlobHeader env)
	    || (globDeclaresValue v $ envGlobProg   env) 
	   then	fromMaybe xx $ makeCall env vsTailCall xx [] tPure
	   else xx
	
	| XDo ss		<- xx
	= let	initSS		= init ss
		Just lastS	= takeLast ss
				
		initSS'		= map (curryS env []) initSS
		lastS'		= curryS env vsTailCall lastS

	  in	XDo (initSS' ++ [lastS'])
			

	-- Application to a literal
	| XAPP XLit{} (TVar kR _)	<- xx
	, kR == kRegion
	= xx
	
	-- Found a function application
	--	split out its arguments and make the call.	
	| (xx =@= XAPP{})
	  || (xx =@= XApp{})
	  
	= let	(parts, effs)	= unzip $ splitApps xx
		(xF:args)	= parts
	  in	fromMaybe xx
			$ makeCall env vsTailCall xF args (makeTSum kEffect effs)

	-- uh oh..			
	| otherwise	
	= panic stage
	$ "curryX: no match for " % show xx % "\n"

	where 	downX	= curryX env vsTailCall
		downA	= curryA env vsTailCall


-- | Rewrite function applications in this alternative
curryA env vsTailCall aa
 = case aa of
 	AAlt gs x	-> AAlt 
				(map (curryG env vsTailCall) gs) 
				(curryX env vsTailCall x)


-- | Rewrite function applicatoin in this guard
curryG env vsTailCall gg
 = case gg of
	GExp  w x	-> GExp w
	 			(curryX env [] x)


-- | Make a call to a function.
--	We need to decide whether we have less than, enough, or more args than the super wants.
--	If less then, 	then we make a partial application.
--	If enough,	then we can call the super directly.
--	If more than,	then we call the super then make a parial application for the rest.
--
makeCall 	
	:: Env			
	-> [Var]		-- ^ supers that can be tailcalled from here
	-> Exp			-- ^ call this function (must be an XVar)
	-> [Exp] 		-- ^ args to function
	-> Effect 		-- ^ effect caused by calling this function
	-> Maybe Exp

makeCall env vsTailCall xF@(XVar vF tF) args eff
 = let	callArity	= length	
			$ filter (\x -> x == True)
			$ map isValueArg 
			$ args
			
	result
 	 -- Function is a top-level super.
	 | Just superArity	
		<- takeFirstJust [ bindingArityFromGlob vF $ envGlobHeader env
				 , bindingArityFromGlob vF $ envGlobProg   env ]
	 = trace	( "* makeCall:\n"
	  		% " f           = "	% vF % "\n"
			% " callArity  = " 	% callArity	% "\n"
			% " superArity = "	% superArity	% "\n")
			$ makeSuperCall xF vsTailCall args eff callArity superArity
				
	 -- Function is represented as a thunk instead of a super.
	 | otherwise
	 =  makeThunkCall xF args eff callArity
	
   in	result
	
makeCall env vsTailCall xF args eff
	= panic stage
	$ "makeCall: no match for " % xF	% "\n"


-- | We've got a top-level supercombinator that we can call directly.
makeSuperCall 
	:: Exp 		-- ^ var of super being called.	(must be an XVar)
	-> [Var]	-- ^ supers that can be tail called.
	-> [Exp] 	-- ^ arguments to super.
	-> Effect 	-- ^ effect caused when evaluating super.
	-> Int 		-- ^ number of args in the call.
	-> Int 		-- ^ number of args needed by the super.
	-> Maybe Exp

makeSuperCall 
	xF@(XVar vF tF)
	tailCallMe args eff callArity superArity
 
	-- The values of CAFs are shared between calls to them..
	--	The Sea stages handle initialisation of them.
 	| superArity	== 0
	, callArity 	== 0
	, not $ Var.isCtorName vF
	= Just xF

	-- We've got the exact number of args the super needs and we're ok
	-- 	for a tail-call. 
 	| callArity == superArity
	, elem vF tailCallMe
	= Just $ XPrim MTailCall (xF : args)

	-- We're not able to do a tail call, but we've still got the right number
	--	of arguments, so we can call the super directly.
	| callArity == superArity
	= Just $ XPrim MCall (xF : args)

	-- We haven't got enough args to call the super yet, we'll have to build
	--	a thunk and wait for more.
	| callArity <  superArity
	= Just $ XPrim (MCurry superArity) (xF : args)

	-- We've got more args than the super will accept.
	--	For this case to be well typed, the super must be returning a thunk.
	--	XCallApp instructs the runtime system to call the super to get the thunk
	--	and then apply the rest of the arguments to it.
	| callArity > superArity
	= Just $ XPrim (MCallApp superArity) (xF : args)
   	
	
-- | Apply a thunk to a value to get the result
makeThunkCall :: Exp -> [Exp] -> Effect -> Int -> Maybe Exp
makeThunkCall xF args eff callArity

	-- If there were only type applications, but no values being applied, 
	--	then the callAirity is zero and there is no associated call at Sea level.
	| callArity == 0
	= Nothing
	
	-- Otherwise we have actual arguments being applied to a thunk.
	| otherwise
	= Just $ XPrim MApply (xF : args)


-- | Checks if this expression represents a value
--	(instead of a type)
isValueArg :: Exp -> Bool
isValueArg xx
 = case xx of
	XLit{}			-> True

 	XVar v t
	 | Var.nameSpace v	== NameValue
	 -> True

	XAPP x t		-> isValueArg x
	
	XType{}			-> False
	
	_	-> panic stage 
			$ "isValueArg: unexpected arg in function application " % xx 
	
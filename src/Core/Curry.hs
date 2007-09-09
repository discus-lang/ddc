-- Core.Curry
--	Works out how to perform function applications and rewrites them into
--	super-call/curry/apply/tail-calls
--
module Core.Curry
	( curryTree 
	, slurpSupersTree 
	, isCafP_opType )

where

-----
import qualified Util.Map	as Map
import Util.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

-----
import Util
import qualified Debug.Trace	as Debug

import qualified Shared.Var	as Var
import Shared.Var		(NameSpace(..))
import qualified Shared.VarUtil	as Var
import Shared.VarPrim
import Shared.Error
import Shared.Exp

import qualified Main.Arg	as Arg
import Main.Arg			(Arg)

import Core.Exp
import Core.Bits
import Core.Util
import Core.Pretty
import Core.Plate.Trans

-----
stage		= "Core.Curry"
debug		= False
trace s	x 	= if debug then Debug.trace (pretty s) x else x

----- 

type CurryM	= State Int

-----
curryTree 
	:: (?args :: [Arg])
	-> Tree			-- ^ headerTree
	-> Tree			-- ^ coreTree
	-> Map Var Top		-- ^ supercombinators which are directly callable
				--	(obtained via slurpSupersTree)
	-> Tree 

curryTree 
	headerTree	
	coreTree
	supers

 = let	?supers		= supers
 	?superVars	= Set.fromList
			$ Map.keys supers
	 
   in	map curryP coreTree


-----
curryP 	p
 = case p of
 	PBind v x
	 | elem Arg.OptTailCall ?args
	 -> PBind v (curryX [v] x)
	 
	 | otherwise
	 -> PBind v (curryX [] x)
	 
	PData v vs ctors
	 -> PData v vs (map curryCtorDef ctors)
	 
	_ -> p
	
-----
curryCtorDef (CtorDef v dataFields)
 = CtorDef v (map curryDataField dataFields)
	
curryDataField field
 = field { dInit = liftM (curryX []) (dInit field) }

-----
curryS 	:: (?supers 	:: Map Var Top)
	-> (?superVars 	:: Set Var)
	-> [Var]		-- supers to tail-call when evaluating this stmt.
	-> Stmt -> Stmt

curryS	tc s
 = case s of
	SComment{}	-> s
	SBind v x	-> SBind v (curryX tc x)
   
-----
curryX	tc xx

	-- boilerplate. it would be nice to ditch some of this
	| XLAM v k x		<- xx	= XLAM v k (curryX tc x)
	| XLam v t x eff clo	<- xx	= XLam v t (curryX tc x) eff clo
	| XAnnot n x		<- xx	= XAnnot n 	$ curryX tc x
	| XTau t x		<- xx	= XTau t	$ curryX tc x
	| XTet vts x		<- xx	= XTet 	 vts	$ curryX tc x
	| XLocal v vs x		<- xx	= XLocal v vs	$ curryX tc x
	| XMatch aa	eff	<- xx	= XMatch (map (curryA tc) aa) eff
	| XPrim{}		<- xx	= xx
	| XConst{} 		<- xx	= xx

	-- A zero airity super.
	| XVar v		<- xx
	= if Set.member v ?superVars
	   then	fromMaybe xx $ makeCall v tc [] TPure
	   else xx
	
	| XDo ss		<- xx
	= let	initSS		= init ss
		Just lastS	= takeLast ss
				
		initSS'		= map (curryS []) initSS
		lastS'		= curryS tc lastS

	  in	XDo (initSS' ++ [lastS'])
			
	
	-- Found a function application
	--	split out its arguments and make the call.	
	| (xx =@= XAPP{})
	  || (xx =@= XApp{})
	  
	= let	(parts, effs)	= unzip $ splitApps xx
		(xF:args)	= parts
		vF		= case xF of
					XVar v	 -> v
					_	-> panic stage
						$ "curryX: malformed exp " % xx
	  in	fromMaybe xx
			$ makeCall vF tc args (makeSumT KEffect effs)

	-- uh oh..			
	| otherwise	
	= panic stage
	$ "curryX: no match for " % show xx % "\n"

-----
curryA tc aa
 = case aa of
 	AAlt gs x	-> AAlt (map (curryG tc) gs) (curryX tc x)

curryG tc gg
 = case gg of
	GExp  w x	-> GExp w (curryX [] x)

-----
makeCall 
	:: (?supers :: Map Var Top)	
	-> Var 					-- call this function
	-> [Var]				-- supers that can be tailcalled from here
	-> [Exp] 				-- args to function
	-> Effect 				-- effect caused by calling this function
	-> Maybe Exp

makeCall vF tc args eff
 
 	-- Function is a top-level super.
	-- 
 	| Just p	<- Map.lookup vF ?supers
	, typeOp	<- superOpTypeP p
	
	= let
	 	callAirity	= length	
				$ filter (\x -> x == True)
				$ map isValueArg 
				$ args

		superAirity	= (length $ flattenFun typeOp) - 1
	
	  in	trace	( "* makeCall:\n"
	  		% " f           = "	% vF % "\n"
			% " callAirity  = " 	% callAirity	% "\n"
			% " superAirity = "	% superAirity	% "\n")
	  
			$ makeSuperCall vF tc args eff callAirity superAirity
	

	-- Function isn't a super.. It'll be a lambda bound function, 
	--	represented as a thunk.
	--
	| otherwise
	= let
	 	callAirity	= length	
				$ filter (\x -> x == True)
				$ map isValueArg 
				$ args
	
	  in	makeThunkCall vF args eff callAirity
	

-----------------------
-- makeSuperCall
--
makeSuperCall 
	:: Var 		-- var of super being called.
	-> [Var]	-- supers that can be tail called.
	-> [Exp] 	-- arguments to super.
	-> Effect 	-- effect caused when evaluating super.
	-> Int 		-- number of args in the call.
	-> Int 		-- number of args needed by the super.
	-> Maybe Exp

makeSuperCall vF tailCallMe args eff callAirity superAirity
 
	-- A reference to a CAF, with no applied arguments.
 	| superAirity	== 0
	, callAirity 	== 0
	, not $ Var.isCtorName vF
	= Just 	$ XVar vF

	-- Arguments applied to a CAF
--	| superAirity 	== 0
--	, callAirity	> 0
--	= Just	$ XPrim (MApply vF) args eff

	-- We've got the exact number of args the super needs and we're ok
	-- 	for a tail-call. 
	--
 	| callAirity == superAirity
	, elem vF tailCallMe
	= Just $ XPrim (MTailCall vF) args eff

	-- We're not able to do a tail call, but we've still got the right number
	--	of arguments, so we can call the super directly.
	--
	| callAirity == superAirity
	= Just $ XPrim (MCall vF) args eff

	-- We haven't got enough args to call the super yet, we'll have to build
	--	a thunk and wait for more.
	--
	| callAirity <  superAirity
	= Just $ XPrim (MCurry vF superAirity) args TPure

	-- We've got more args than the super will accept.
	--	For this case to be well typed, the super must be returning a thunk.
	--	XCallApp instructs the runtime system to call the super to get the thunk
	--	and then apply the rest of the arguments to it.
	--
	| callAirity > superAirity
	= Just $ XPrim (MCallApp vF superAirity) args eff
   	
	
	
-----------------------
-- makeThunkCall
--
makeThunkCall ::	Var -> [Exp] -> Effect -> Int -> Maybe Exp
makeThunkCall		vF 	args    eff	 callAirity

	-- If there were only type applications, but no values being applied, 
	--	then the callAirity is zero and there is no associated call at Sea level.
	--
	| callAirity == 0
	= Nothing
	
	-- Otherwise we have actual arguments being applied to a thunk.
	--
	| otherwise
	= Just $ XPrim (MApply vF) args eff


-----
--
isValueArg :: Exp -> Bool
isValueArg xx
 = case xx of
 	XVar v
	 | Var.nameSpace v	== NameValue
	 -> True

	XAPP x t		-> isValueArg x
	
	XType{}			-> False
	
	_	-> panic stage 
			$ "isValueArg: unexpected arg in functio application " % show xx 
	
	
-----
-- slurpSupersTree
--	Slurp out the list of functions which can be called directly as supers.
--
slurpSupersTree :: Tree -> Map Var Top
slurpSupersTree tree
 	= Map.fromList 
 	$ catMaybes 
	$ map slurpTopP 
	$ tree	 	

slurpTopP p
 = case p of
 	PBind v _	-> Just (v, p)
	PCtor  v _ _	-> Just (v, p)
	PExtern v tv to	-> Just (v, p)
	_		-> Nothing


-----
-- isCafP_opType
--	Inspects the operational type of a top to see if it declares a CAF.
--	Supers must be annotated with operational types.
--
--
isCafP_opType :: Top -> Bool
isCafP_opType p	
 = case p of

	-- Treat a binding as a CAF if it has no args.
	--
 	PBind{}		-> length (flattenFun $ superOpTypeP p) == 1

	-- Ctors are never CAFs.
	--	eg, we want a new True object every time we call True, because it might get updated later.
	--	The Atomise optimisation will increase sharing for Constant objects.
	--
	PCtor v tv to	-> False

	-- Non-function unboxed data imported via a foreign import is not a CAF.
	--	This lets us import top-level C values directly into our program.
	--	eg foreign import extern "SEEK_SET" seek_set :: Int32#;
	-- 
	PExtern v tv to	
	 -> case flattenFun to of
		[x]		-> not $ isUnboxedT x
		_		-> False
	 
	-- Nothing else is a CAF
	_ 		-> False
	
	


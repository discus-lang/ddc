
module Type.Check.CheckPure
--	( checkPure )
	()

where

import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Data.Array.IO

import qualified Util.Data.Bag	as Bag
import Util.Data.Bag			(Bag)

import qualified Shared.Var	as Var
import Shared.Error

import Type.Exp
import Type.Error
import Type.Util

import Type.State
import Type.Class
import Type.Scheme
import Type.Trace.TraceFetters	(traceFettersZ)

-----
stage	= "Type.Squid.CheckPure"
debug	= False
trace s	= when debug $ traceM s
{-
-----
-- checkPure
--	| Check that the purity fetters acting on effect classes are satisfied.
--	
--	From each effect class containing manifest effects, track back 
--	though the graph and see if there are any effect classes referencing
--	this one which include the Pure fetter.
--
--	We only need to check for top level effects here. If we have purity fetters
--	acting on non maskable Read or Write effects then these will be caught
--	in Squid.CheckConst.
--
checkPure :: SquidM [Error]
checkPure
 = do 	
 	-- We only care about effect classes.
 	school		<- gets stateClass 
	classes		<- liftIO (getElems $ schoolClass school)
	let csEff	= [c 	| c@Class{} <- classes
				, classKind c == KEffect]

	errs		<- liftM concat
			$ mapM checkPureC csEff
	
	return errs
		

-----------------------
-- checkPureC
--	Check that this effect class satisfies any purity constraints.	
--
checkPureC :: 	Class -> SquidM [Error]
checkPureC	c
 = do	let cid		= classId c
 	 
	-- Gather up all the effects which are local to this class.
	let esHere	= catMap crushEffsTS 
			  $ classNodes c
			
	-- Collect up the non-maskable effects.
	--	Non maskable effects are top level effects (ones with no args),
	--	and Write effects.
	--
	let esTop	= [e	| e@(ECon v [], _) <- esHere]

	let esWrite	= [e	| e@(ECon v _, _)  <- esHere
				, Var.bind v == Var.EWrite]
	
 	trace	$ "*   CheckPure.checkPureC " % cid 	% "\n"
		% "    esHere  = " % esHere		% "\n"
		% "    esTop   = " % esTop		% "\n"
		% "    esWrite = " % esWrite		% "\n"
		% "\n"
	
	let esAll	= esTop ++ esWrite

	case esAll of
	 []		-> return []
	 _		-> checkPureEffs cid esAll

	 
-----
-- checkPureEffs
--	Ok, we've got some top-level effects.
--	Make sure something isn't trying to purify them.
--
checkPureEffs
	:: ClassId
	-> [(Effect, TypeSource)]
	-> SquidM [Error]

checkPureEffs cid esTop
 = do	
 	cidFs		<- traceFettersZ
				Set.empty
				(Bag.singleton cid)
				[]

	let fsPure	= [f 	| f@(_, FClass v _) <- cidFs
				, Var.bind v == Var.FPure]

	trace	$ "    cidFs   = " % cidFs		% "\n"
		% "    fsPure  = " % fsPure		% "\n"
		% "\n"
		
	case fsPure of
	 []		-> return []
	 _		-> checkPureError cid esTop fsPure

	 
-----
-- checkPureError
--	We've found a problem. 
--	See where the Purity constraint came from and build the error.
--
checkPureError
	:: ClassId
	-> [(Effect, TypeSource)]
	-> [(ClassId, Fetter)]
	-> SquidM [Error]

checkPureError 
	cid 
	esTop@((e, eInfo):_)
	fsPure@((cidPure, FClass v _) : _)
	
 = do
	-- Lookup the source of the pure fetter.
	Just cPure	<- lookupClass cidPure
	
	let  ((TFetter p, pInfo) : _) = 
		[x 	| x@((TFetter (FClass v _)), _)	<- classNodes cPure
			, Var.bind v == Var.FPure]
				
	-- Report the error
	-- 	We only report a single effect here, even if there are 
	--	multiple top-level effs in the purified class.
	--
	return [ErrorCannotPurify
  		{ eEffect	= e
		, eEffectSource	= eInfo
		, eFetter	= p
		, eFetterSource	= pInfo }]

-----------------------
-- crushEffs
--
crushEffsTS
	:: (Type, TypeSource)	
	-> [(Effect, TypeSource)]

crushEffsTS x@(t, tInfo)
 = case t of
 	TEffect (ESum es)	-> zip es $ repeat tInfo
	_			-> []


	
-}

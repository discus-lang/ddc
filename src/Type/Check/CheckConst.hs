module Type.Check.CheckConst
(
--	checkConst
)

where

-----
import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Data.Array.IO


import qualified Util.Data.Bag	as Bag
import Util.Data.Bag		(Bag)

-----
import qualified Shared.Var	as Var
import Shared.Error

import Type.Exp
import Type.Util
import Type.Error

import Type.State
import Type.Class
import Type.Scheme
import Type.Induced

import Type.Trace.TraceFetters
import Type.Trace.TraceEffects

-----
stage	= "Type.Squid.CheckConst"
debug	= True
trace x	= when debug $ traceM x

{-
-----
-- checkConst
--	| For all regions in the graph, check that they aren't trying to
--	  be both Const and Mutable at the same time.
--
--	BUGS: we also have to do this for type nodes so we can catch
--	Const t0, Mutable t0 problems.
--
checkConst :: SquidM [Error]
checkConst	
 = do
	-- We only care about region classes.
	school		<- gets stateClass
	classes		<- liftIO (getElems $ schoolClass school)
	let csRegion	= [ c 	| c@Class{} <- classes
				, classKind c == KRegion]
				
	errs		<- liftM concat
			$  mapM checkConstC csRegion
			
	return errs
	

	
checkConstC :: Class -> SquidM [Error]
checkConstC    c
 = do	
 	let cid		= classId c
 
	-- Find the classes which contain effects that are acting on this one.
	cidsEs		<- traceEffectsZ
				Set.empty
				(Bag.singleton cid)
				[]
	
	cidClass	<- mapM (\cid -> do
					Just c	<- lookupClass cid
					return (cid, c))
			$ cidsEs
	

	-- Build a list containing:
	--	(which class an effect is in, the effect, the source of the effect).
	--

	let cidEffSource
		= (concat.concat)
		$ map (\(cid, c) -> 
			[zip3 (repeat cid) es (repeat (classTypeSource c))
				| TEffect (ESum es) <- classQueue c ])
		$ cidClass

	cidEffSourceU_
		<- mapM (\(cid, eff, tSource)
		 	-> do	effU	<- updateVC eff
				return (cid, effU, tSource))
		$ cidEffSource

	-- This sometimes includes more effects than it should.
	--	Crushing effs like EReadH can eliminate the effect, but not the backref from the
	--	type class that was being read.
	-- Restrict the effect list to only the effects that are actually on the region

	let cidEffSourceU	
		= [ ces 	| ces@(_, ECon _ [TRegion (RClass cidR)], _)
				<- cidEffSourceU_ 
				, cidR == cid ]

	---------------
	-- Do the ConstWrite check.
	--
	fetterCids	<- liftM (map fst)
			$  traceFettersZ
				Set.empty
				(Bag.singleton cid)
				[]

	fetterCs	<- liftM (map (\(Just c) -> c))
			$  mapM lookupClass fetterCids
			
	let fetterNodes	= catMap classNodes fetterCs


	-- Const fetters in this class.
	let constDirect
	 	= [(f, tSource)
			| (TFetter f@(FClass v _), tSource)	
				<- fetterNodes
			, Var.bind v == Var.FConst ]

	-- Write effects on this class.
	let effsWrite
	 	= [(e, tSource)
			| (_, e@(ECon v _), tSource)
				<- cidEffSourceU
			, elem (Var.bind v) [Var.EWrite, Var.EWriteT] ]

	errsConstWrite
		<- checkConstWrite 
			constDirect
			effsWrite


	---------------
	-- Do the PureReadWrite check.
	--

	-- Read effects on this class.
	let effsRead
		= [(cid, e, tSource)
			| (cid, e@(ECon v _), tSource)
				<- cidEffSourceU
			, elem (Var.bind v) [Var.ERead, Var.EReadT, Var.EReadH] ]

	errsPureReadWrite
		<- checkPureReadWrite 
			effsRead
			effsWrite


	---------------
	-- Debugging
	--

	trace	$ "*   CheckConst.checkConstC " % cid 		% "\n"
		% "    cidsEs          = " % cidsEs		% "\n"
		% "    cidEffSourceU   = " % cidEffSourceU	% "\n"
		% "    fetterCids      = " % fetterCids		% "\n"
		% "    constDirect     = " % constDirect	% "\n"
		% "    effsWrite       = " % effsWrite		% "\n"
		% "    effsRead        = " % effsRead		% "\n"
		% "\n"

	return	$  errsConstWrite 
		++ case errsPureReadWrite of
			(x:xs)	-> [x]
			[]	-> []
			

-- classTypeSource
--	Choose a TypeSource from one of the nodes in this class to represent it.
--	
classTypeSource :: Class -> TypeSource
classTypeSource c
 = let	ts	= map snd 
 		$ classNodes c
		
   in	selectTypeSource ts
	
	
-- selectTypeSource
-- 	Choose the first non TSNil from a list of TypeSources
--
selectTypeSource :: [TypeSource] -> TypeSource	
selectTypeSource xx
 = case xx of
 	(TSNil : xs)	-> selectTypeSource xs
	(x:xs)		-> x
	[]		-> TSNil

-----
-- checkConstWrite
--	Check that if this region contains Const fetters, 
--	then there are no writes to it.
--
--	We'll take the _last_ source in the list of type sources
--	to report the location of the error as this should correspond
--	to the source of the _first_ conflicting constraint in the source file.
--
checkConstWrite constDirect mutableWrite
	| (constF, constSource) : _	<- reverse constDirect
	, (writeE, writeSource) : _	<- reverse mutableWrite
	= do	
		constF'	<- updateVC constF
		writeE'	<- updateVC writeE
	
		return	$ [ErrorConstWrite
			{ eEffect	= writeE'
			, eEffectSource	= writeSource
			, eFetter	= constF'
			, eFetterSource	= constSource }]
	
	| otherwise
	= return []
	

-----
-- checkPureReadWrite
--	Check that if this region has both Read and Write
--	effects acting on it, then none of the Reads are
--	being purified.
--
checkPureReadWrite effsRead effsWrite

  | []				<- effsWrite
  = return []

  | (writeE, writeSource) : _	<- effsWrite 
  = do
 	pureReads	<- liftM catMaybes
			$  mapM checkPureRead effsRead
	
	case pureReads of
	 []		
	  -> return []

	 (cid, readE, readSource, (pureF, pureSource)) : _
	  -> return	$ [ErrorPureReadWrite
	  		{ eReadEff	= readE
			, eReadSource	= readSource
			, ePureFetter	= pureF
			, ePureSource	= pureSource
			, eWriteEff	= writeE
			, eWriteSource	= writeSource }]

			
----
-- checkPureRead
--	For this read effect, see if it is being purified.
--	If so, add this information to the tuple.
--
checkPureRead 
	(cid, eff, ts)
 = do
 	-- Find out what fetters are above this class.
	--
	fetters		<- traceFettersZ
				Set.empty
				(Bag.singleton cid)
				[]

	-- See if there were any Pure fetters above us.
	--
	let cFs		= [ cFs	| cFs@(cidF, FClass v _) <- fetters
				, Var.bind v == Var.FPure]

	case cFs of
	 []	-> return Nothing
	 (x:xs) -> checkPureRead2 cid eff ts x
	 

checkPureRead2 cid eff effTS (cidF, fetter)
 = do
	-- If there were Pure fetters, traceFettersZ doesn't return
	--	typeSources, so look them back up.
	--
 	Just c	<- lookupClass cidF

	let fsPure	= [ (f, ts)
				| (TFetter f@(FClass v _), ts)
					<- classNodes c
				, Var.bind v == Var.FPure ]
				
	let mPurifier	= takeHead fsPure

	trace	$ "*   checkPureRead\n"		
		% "    cid               = " % cid 		% "\n"
		% "    eff               = " % eff		% "\n"
		% "    fsPure            = " % fsPure 		% "\n"
		% "    mPurifier         = " % mPurifier 	% "\n\n"

	case mPurifier of
	 Nothing	-> return Nothing
	 Just purifier	-> return $ Just (cid, eff, effTS, purifier)
	 
-}


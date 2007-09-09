
module Type.Induced
(
--	addInducedFsT,
--	getFettersZ,
)

where

import Util

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Util.Data.Bag	as Bag
import Util.Data.Bag		(Bag)

import qualified Shared.Var	as Var
import Shared.Error
import Shared.VarPrim

import Type.Exp
import Type.Util
import Type.Plate

import Type.State
import Type.Class

import Type.Trace.TraceFetters
import Type.Trace.TraceEffects

-----
stage	= "Type.Squid.Induced"
debug	= False
trace s	= when debug $ traceM s

{-
-----------------------
-- addInducedFsT
--
addInducedFsT :: Type -> SquidM Type
addInducedFsT	t
 = do
	let cids	= nub 
			$ collectClassIds t

	fs		<- liftM concat
			$  mapM getFettersZ cids

	let t'		= addFetters t fs

	trace	$ "*   Induced.addInducedFsT\n"
		% "    t    = " % t 	% "\n"
		% "    cids = " % cids	% "\n"
		% "\n"
	
	return t'



-----------------------
-- getFettersZ
--	Get all the fetters on this class
--
getFettersZ :: 	ClassId -> SquidM [Fetter]
getFettersZ	cidZ
 = do	Just c	<- lookupClass cidZ
	case classKind c of
	 KType		-> getFettersInducedRT cidZ
 	 KRegion	
	  -> do	fsI	<- getFettersInducedRT 	cidZ
	 	fsD	<- getFettersDirectR	cidZ
		return	$ nub $ fsI ++ fsD

	 KEffect	-> getFettersE cidZ
	 KClosure	-> return []


getFettersInducedRT	cidZ
 = do
	Just c		<- lookupClass cidZ
	let kind	= classKind c

	-- Determine the effects reachable by tracing back from this class.
 	cidEs	<- traceEffectsZ Set.empty (Bag.singleton cidZ) []

	eCons	<- liftM concat
		$  mapM (getEConsRef cidZ) cidEs

	-- Determine the effect fetters acting on each of these effects.
	fsEff	<- mapM (\cidE -> liftM (map snd) 
					$ traceFettersZ
						Set.empty
						(Bag.singleton cidE)
						[])
		$ cidEs

	-- Determine the type fetters reachable by tracing back from this class.
	fsType	<- liftM nub
		$  liftM (map snd)
		$  traceFettersZ Set.empty (Bag.singleton cidZ) []


	-- See if this class is being forced to be const.
	let constEFS
		= or 
		$ map forceConstEFS 
		$ zip eCons fsEff
	
	constRH	<- liftM or
		$  mapM (forceConstRH cidZ) 
		$  zip eCons fsEff
	
	let fsConst
		= if constEFS || constRH
			then 	[makeConstF kind cidZ]
			else	[]
			

	-- See if this class is being forced to be mutable.
	let mutable
		=  or (map isMutableF fsType)	-- if we have a MutableT on a type above us.
		
			
	let fsMutable 
		= if mutable
			then	[makeMutableF kind cidZ]
			else	[]
		
	-----
	let fsAll	= fsMutable ++ fsConst
		
	let junk	= zip3 cidEs eCons fsEff
	trace	$ "*   Induced.getFettersZ\n"
		% "    cidZ       = " % cidZ 		% "\n"
		% "    cidEs      = " % cidEs		% "\n"
		% "    junk       = " % junk		% "\n"
		% "    constEFS   = " % constEFS	% "\n"
		% "    constRH    = " % constRH		% "\n"
		% "    mutable    = " % mutable		% "\n"
		% "    fsType     = " % fsType		% "\n"
		% "    fsAll      = " % fsAll		% "\n\n"

	return fsAll


getFettersDirectR cidR
 = do
	Just c		<- lookupClass cidR
--	let fsDirect	= classFetters c
	let fsDirect	= []
	fsDirectU	<- updateVC fsDirect
	return	fsDirectU


getFettersE	cidE
 = do
	-- Determine the effect fetters acting on this effect
	fsEff	<- liftM (map snd)
		$  traceFettersZ
			Set.empty
			(Bag.singleton cidE)
			[]

	-- See if this class is being forced pure.
	let pure
		= or 
		$ map isPureF fsEff
		
	let fsPure
		= if pure
			then 	[makePureF KEffect cidE]
			else	[]
			
	return fsPure

-----
-- getEConsRef
--	Get ECons from an effect class which refer to some other class.
--
getEConsRef ::	ClassId -> ClassId -> SquidM [Effect]
getEConsRef	cidRef     cidE
 = do
 	Just c	<- lookupClass cidE
	
	let [TEffect (ESum es)]	
		= classQueue c

	esU	<- updateVC es
	
	let esR	= catMaybes
		$ map (\e -> case e of
				ECon v [TRegion (RClass cidR)]
				 | cidRef == cidR	-> Just e
				_ 			-> Nothing)
		$ esU

	return esR				
					


forceConstEFS :: (Effect, 	[Fetter])	-> Bool
forceConstEFS	(ECon v _, 	fs)
	=  (or $ map isPureF fs)
	&& Var.name v `elem` ["Read", "ReadT", "Write", "WriteT"]


forceConstRH ::	 ClassId -> (Effect,	[Fetter])	-> SquidM Bool
forceConstRH	 cidR	    efs
 = do	Just c		<- lookupClass cidR
 	let kind	=  classKind c

 	forceConstRH' kind cidR efs

forceConstRH'	kind	cidR	(ECon v ts, fs)	

	| KRegion	<- kind
	, or $ map isPureF fs
	, Var.name v == "ReadH"
	, [TClass cidT]	<- ts
	= do
		Just c		<- lookupClass	cidT
		let [t]		= classQueue c
				
		case t of
		 TCon _ (TRegion (RClass cidR') : _)
		  ->	return	$ cidR == cidR'
		  
		 _ ->	return False


	| otherwise
	= return False
	
-----------------------
makeConstF ::	Kind -> ClassId -> Fetter
makeConstF	kind cidZ
 = case kind of
	KType		-> FClass primConstT [TClass cidZ]
 	KRegion		-> FClass primConst  [TRegion (RClass cidZ)]


makeMutableF ::	Kind -> ClassId -> Fetter
makeMutableF	kind cidZ
 = case kind of
	KType		-> FClass primMutableT [TClass cidZ]
 	KRegion		-> FClass primMutable  [TRegion (RClass cidZ)]
	

makePureF ::	Kind -> ClassId	-> Fetter
makePureF	kind cidE
 = case kind of
 	KEffect		-> FClass  primPure [TEffect (EClass cidE)]
		
-}		

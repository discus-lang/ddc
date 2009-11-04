
module Sea.Slot
(
	slotTree

)

where

-----
import qualified Debug.Trace	as Debug
import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var
import Shared.Var		(Var, NameSpace(..))

import qualified Shared.Unique	as Unique

import Shared.Error
import Shared.VarUtil

import Sea.Exp
import Sea.Pretty
import Sea.Util
import Sea.Plate.Trans


-----
--	
slotTree ::	Tree () -> Tree () -> Set Var -> Tree ()
slotTree	tree	eHeader cafVars
 = let	
{- 	cafVarsLocal	= Set.fromList [ v | PCafSlot v 	<- tree]
	cafVarsImport	= Set.fromList [ v | PProto v [] t	<- eHeader]
	cafVars		= cafVarsLocal `Set.union` cafVarsImport
-}
   in	
   	evalState (mapM (slotP cafVars) tree) 	
 	$ Var.XBind Unique.seaSlot 0


slotP 	:: Set Var	
	-> Top () -> VarGenM (Top ())

slotP	cafVars p
 = case p of
 	PSuper v args retT ss
	 -> do	(ssAuto, ssArg, ssR, slotCount)	
	 		<- slotSS cafVars args ss

		let Just (SReturn x) = takeLast ssR
		retVar		<- newVarN NameValue
		
		let retBoxed		= not $ typeIsUnboxed retT
		let hasSlots		= slotCount > 0

		return	$ PSuper v args retT
			$  (nub ssAuto)
			++ (if retBoxed	&& hasSlots 	then [ SAuto	retVar retT] 		else [])
			++ (if hasSlots			then [ SEnter	slotCount  ] 		else [])
			++ ssArg
			++ init ssR
			++ (if retBoxed && hasSlots	then [ SAssign	(XVar retVar retT) retT x ]	else [])
			++ (if hasSlots			then [ SLeave	slotCount ] 		else [])
			++ (if retBoxed && hasSlots
				then [ SReturn (XVar retVar retT) ]
				else [ SReturn x ])

	_ -> return p


slotSS 	:: Set Var -> [(Var, Type)] -> [Stmt ()] 
	-> VarGenM 
		( [Stmt ()]
		, [Stmt ()]
		, [Stmt ()]
		, Int)
		
slotSS cafVars args ss
 = do	
 	-- Place the managed args into GC slots.
	let (ssArg_, state0)
		= runState 
			(mapM slotAssignArg args)
			slotInit
	
	let ssArg	= catMaybes ssArg_
			
	-----
	-- Create a new GC slots for vars on the LHS of an assignment.
	--
	let (ss2, state1)
		= runState 
			(mapM (transformSM slotAssignS) ss) 
			state0

	-- Rewrite vars in expressions to their slots.
	let ss3	= map (transformX 
			(\x -> slotifyX x (stateMap state1) cafVars))
			ss2

	-- Make auto's for unboxed data.
	--	In compiled tail recursive functions there may be assignments
	--	to arguments in the parameter list. These will appear in the stateAuto 
	--	list, but there's no need to actually make a new auto for them.
	--
	let ssAuto	= [ SAuto v t	| (v, t)	<- reverse
							$ stateAuto state1
					, not $ elem v	$ map fst args]

	let slotCount	= Map.size (stateMap state1)
					
	return	(ssAuto, ssArg, ss3, slotCount)

slotAssignArg	(v, t)
 	| typeIsUnboxed t
	= return Nothing
	
	| otherwise
	= do
	 	slot	<- newSlot
		let exp	= XSlot v t slot
		addSlotMap v exp
		
		return	$ Just
			$ SAssign exp t (XVar v t)
		
slotifyX x m cafVars
	| XVar v _	<- x
	, Just exp	<- Map.lookup v m
	= exp
	
	| XVar v t	<- x
	, Set.member v cafVars
	= XVarCAF v t
	
	| otherwise
	= x

-----
data SlotS
	= SlotS
	{ stateMap	:: Map Var (Exp ())
	, stateSlot	:: Int 
	, stateAuto	:: [(Var, Type)] }

slotInit
	= SlotS
	{ stateMap	= Map.empty
	, stateSlot	= 0 
	, stateAuto	= [] }
	
 
type SlotM = State SlotS

newSlot :: SlotM Int
newSlot	
 = do 	slot	<- gets stateSlot
	modify (\s -> s { stateSlot = slot + 1 })
	return slot
	

addSlotMap :: Var -> Exp () -> SlotM ()
addSlotMap    var    x
 =	modify (\s -> s { 
 		stateMap = Map.insert var x (stateMap s) })




-----
-- slotAssignS
--	Assign a GC slot for all variables present on the LHS of an assignment.
--
slotAssignS ::	Stmt () -> SlotM (Stmt ())
slotAssignS	ss
 = case ss of
 	SAssign (XVar v t) TObj x
	 -> do 	slotMap	<- gets stateMap
	 
	 	case Map.lookup v slotMap of
		 Nothing 
		  -> do	slot	<- newSlot
			let exp	= XSlot v t slot
			addSlotMap v exp

			return	$ SAssign exp TObj x
		
		 Just exp
		  ->	return	$ SAssign exp TObj x

	SAssign (XVar v _) t x
	 -> do	modify (\s -> s {
	 		stateAuto = (v, t) : stateAuto s })
		return ss

	_ 	-> return ss



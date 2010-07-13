
-- | Rewrite code so that it stores pointers to boxed objects on the GC slot stack
--	instead of directly in C variables. We do this so that the garbage collector
--	can work out the root set if called.
module Sea.Slot
	(slotTree)
where
import Util
import Shared.VarUtil
import Sea.Exp
import Sea.Util
import Sea.Plate.Trans
import DDC.Var
import qualified DDC.Core.Glob	as C
import qualified Shared.Unique	as Unique
import qualified Shared.VarUtil	as Var
import qualified Data.Map	as Map


-- | Rewrite code in this tree to use the GC slot stack.
slotTree 
	:: Tree () 		-- ^ source tree.
	-> Tree () 		-- ^ prototypes of imported functions.
	-> C.Glob		-- ^ header glob, used to get arities of supers.
	-> C.Glob		-- ^ source glob, TODO: refactor this to use sea glob
	-> Tree ()

slotTree tree eHeader cgHeader cgSource
 	= evalState (mapM (slotP cgHeader cgSource) tree) 	
 	$ VarId Unique.seaSlot 0

-- | Rewrite code in this top level thing to use the GC slot stack.
slotP 	:: C.Glob		-- ^ header glob
	-> C.Glob		-- ^ source glob
	-> Top () 
	-> VarGenM (Top ())

slotP cgHeader cgSource p
 = case p of
 	PSuper v args retT ss
	 -> do	(ssAuto, ssArg, ssR, slotCount)	
	 		<- slotSS cgHeader cgSource args ss

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

-- | Rewrite the body of a supercombinator to use the GC slot stack
slotSS 	:: C.Glob		-- ^ header glob
	-> C.Glob		-- ^ source glob
	-> [(Var, Type)] 	-- ^ vars and types of the super parameters
	-> [Stmt ()] 		-- ^ statements that form the body of the super
	-> VarGenM 
		( [Stmt ()]	-- code that copies the super parameters into their slots.
		, [Stmt ()]	-- code that initialises automatic variables for each local unboxed variable.
		, [Stmt ()]	-- body of the super, rewritten to use slots.
		, Int)		-- the number of GC slots used by this super.
		
slotSS cgHeader cgSource args ss
 = do	
 	-- Place the managed args into GC slots.
	let (ssArg_, state0)
		= runState 
			(mapM slotAssignArg args)
			slotInit
	
	let ssArg	= catMaybes ssArg_
			
	-- Create a new GC slots for vars on the LHS of an assignment.
	let (ss2, state1)
		= runState 
			(mapM (transformSM slotAssignS) ss) 
			state0

	-- Rewrite vars in expressions to their slots.
	let ss3	= map (transformX 
			(\x -> slotifyX x (stateMap state1) cgHeader cgSource))
			ss2

	-- Make automatic variables for unboxed values.
	--	In compiled tail recursive functions there may be assignments
	--	to arguments in the parameter list. These will appear in the stateAuto 
	--	list, but there's no need to actually make a new auto for them.
	let ssAuto	= [ SAuto v t	| (v, t)	<- reverse
							$ stateAuto state1
					, not $ elem v	$ map fst args]

	-- Count how many slots we've used
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
		
slotifyX x m cgHeader cgSource
	| XVar v _	<- x
	, Just exp	<- Map.lookup v m
	= exp
	
	-- BUGS: unboxed top level data is not a CAF
	| XVar v t	<- x
	, isCafVar cgHeader cgSource v
	= XVarCAF v t
	
	| otherwise
	= x

-- | Decide whether a var is represented as a top level CAF.
--	CAFs are bindings with arity 0, and that weren't imported with an explicit Sea name.
--
--	A binding with an explicit Sea name is an external constant, so we don't need
--	to compute the value of it at module startup time (like we do with other CAFs)
--
isCafVar :: C.Glob -> C.Glob -> Var -> Bool
isCafVar cgHeader cgSource v
	| Just 0	<- C.bindingArityFromGlob v cgHeader
	= not $ isJust $ Var.takeSeaNameOfBindingVar v
		
	| Just 0	<- C.bindingArityFromGlob v cgSource
	= not $ isJust $ Var.takeSeaNameOfBindingVar v
	
	| otherwise
	= False


-- SlotM ------------------------------------------------------------------------------------------
-- State monad for doing the GC slot transform.
data SlotS
	= SlotS
	{ stateMap	:: Map Var (Exp ())	-- ^ map of original variable
						--	to the XSlot expression that represents
						--	the GC slot it is stored in.

	, stateSlot	:: Int 			-- ^ new slot number generator

	, stateAuto	:: [(Var, Type)] }

slotInit
	= SlotS
	{ stateMap	= Map.empty
	, stateSlot	= 0 
	, stateAuto	= [] }
	
type SlotM = State SlotS


--- | Create a fresh slot number
newSlot :: SlotM Int
newSlot	
 = do 	slot	<- gets stateSlot
	modify (\s -> s { stateSlot = slot + 1 })
	return slot
	

-- | Add an entry to the slot map
addSlotMap 
	:: Var 
	-> Exp () 
	-> SlotM ()
addSlotMap    var    x
 =	modify (\s -> s { 
 		stateMap = Map.insert var x (stateMap s) })


-- | Assign a GC slot for all variables present on the LHS of an assignment.
slotAssignS ::	Stmt () -> SlotM (Stmt ())
slotAssignS	ss
 = case ss of
 	SAssign (XVar v t) (TPtr TObj) x
	 -> do 	slotMap	<- gets stateMap
	 
	 	case Map.lookup v slotMap of
		 Nothing 
		  -> do	slot	<- newSlot
			let exp	= XSlot v t slot
			addSlotMap v exp

			return	$ SAssign exp (TPtr TObj) x
		
		 Just exp
		  ->	return	$ SAssign exp (TPtr TObj) x

	SAssign (XVar v _) t x
	 -> do	modify (\s -> s {
	 		stateAuto = (v, t) : stateAuto s })
		return ss

	_ 	-> return ss


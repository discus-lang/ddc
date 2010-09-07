
-- | Use data type declarations to make code for each of the data constructors.
module Sea.Ctor
	(expandCtorTree)
where
import Util
import DDC.Sea.Exp
import DDC.Var
import Shared.VarUtil		(VarGenM, newVarN)
import qualified Shared.Unique	as Unique
import qualified Data.Map	as Map

type	ExM	= VarGenM 


-- | Expand the definitions of data constructors in this tree.
expandCtorTree :: Tree () -> Tree ()
expandCtorTree tree
	= evalState (liftM concat $ mapM expandDataP tree) 
	$ VarId Unique.seaCtor 0
	

-- | Expand data constructors in a top level thing.
expandDataP :: Top ()	-> ExM [Top ()]
expandDataP p
 = case p of
 	PData v ctors
	 -> liftM concat 
	  $ mapM (\(v, ctor) -> expandCtor ctor) 
	  $ Map.toList ctors

	_		-> return [p]
	

-- | Expand the definition of a constructor.
expandCtor 
	:: CtorDef
	-> ExM [Top ()]
	
expandCtor (CtorDef vCtor tCtor arity tag fields)
 = do	-- var of the constructed object.
	objV		<- newVarN NameValue

	-- allocate the object
	let allocS 	= SAssign (XVar objV (TPtr TObj)) (TPtr TObj) 
			$ XPrim (MAlloc (PAllocData vCtor arity)) []

	-- Initialise all the fields.
	(stmtss, mArgVs)
		<- liftM unzip $ mapM (expandField objV) [0 .. arity - 1]

	let fieldSs	= concat stmtss
	let argVs	= catMaybes mArgVs

	-- Return the result.
	let retS	= SReturn $ (XVar objV (TPtr TObj)) 

	let stmts	= [allocS] ++ fieldSs ++ [retS]
	let super	= [PSuper vCtor argVs (TPtr TObj) stmts]
	
	return 		$ super
 	

-- | Create initialization code for this field
expandField
	:: Var				-- ^ var of object being constructed.
	-> Int				-- ^ index of argument.
	-> ExM 	( [Stmt ()]		-- initialization code

		, Maybe (Var, Type))	-- the arguments to the constructor
					--	(will be Nothing if the field is secondary)
expandField objV ixArg
 = do	argV	<- newVarN NameValue
	return	( [SAssign 	(XArg (XVar objV (TPtr TObj)) TObjData ixArg)
				(TPtr TObj) 
				(XVar argV (TPtr TObj))]
		, Just (argV, TPtr TObj) )


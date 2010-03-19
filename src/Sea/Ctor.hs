
-- | Use data type declarations to make code for each of the data constructors.
module Sea.Ctor
	(expandCtorTree)
where
import Sea.Exp
import Shared.Var		(Var, NameSpace(..))
import Shared.VarUtil		(VarGenM, newVarN)
import Shared.Error
import qualified Shared.Unique	as Unique
import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var
import qualified Data.Map	as Map
import Data.Map			(Map)
import Util

-----
-- stage	= "Sea.Ctor"

-----
type	ExM	= VarGenM 

-- | Expand the definitions of data constructors in this tree.
expandCtorTree :: Tree () -> Tree ()
expandCtorTree tree
	= evalState (liftM concat $ mapM expandDataP tree) 
	$ Var.XBind Unique.seaCtor 0
	
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
 = do
	-- var of the constructed object.
	objV		<- newVarN NameValue

	-- allocate the object
	let allocS 	= SAssign (XVar objV TObj) TObj 
			$ XAllocData vCtor 
			$ arity

	-- field init
	(stmtss, mArgVs)
		<- liftM unzip $ mapM (expandField objV) [0 .. arity - 1]

	let fieldSs	= concat stmtss
	let argVs	= catMaybes mArgVs

	-- return result
	let retS	= SReturn $ (XVar objV TObj) 

	let stmts	= [allocS] ++ fieldSs ++ [retS]
	let super	= [PSuper vCtor argVs TObj stmts]
	
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
	return	( [SAssign 	(XArg (XVar objV TObj) TObjData ixArg)
				TObj 
				(XVar argV TObj)]
		, Just (argV, TObj) )

--	-- Primary fields get their values from constructor arguments.
--	| dPrimary field

{-
	-- Secondary fields get their values by calling the init function
	| not $ dPrimary field
	, Just vInit		<- dInit field
	= 	return	( [SAssign 	(XArg (XVar objV (dType field)) TData ix) 
					(dType field)
					(XCall vInit [XUnit]) ]
	 		, Nothing )

	-- A secondary field without an initializer
	| not $ dPrimary field
	, Nothing	<- dInit field
	, Just name	<- dLabel field
	= panic stage $ "expandField: no initialization expression for non-primary field '" % name % "'"
-}